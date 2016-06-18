(ns clj-git.core
  (:use clj-message-digest.core)
  (:use clojure.java.io)
  (:use clojure.java.shell)
  (:require [clojure.java.io :as io])
  (:gen-class))

(def SHA-1-HEX-LENGTH 40)

(declare modified)
(defn -main [& args]
  (println "Unstaged Changes:")
  (doseq [x (modified)]
    (println (str "  " x))))

(defn to-str [x] (apply str (map char x)))

(defn current-unix-time []
  (quot (System/currentTimeMillis) 1000))

(defn ok-sh [command & args]
  (let [res (apply sh (cons command args))]
    (assert (= 0 (:exit res)))
    (assert (= "" (:err res)))
    (:out res)))

(defn set-writable [filepath]
  (ok-sh "chmod" "u+w" filepath))

(defn unset-writable [filepath]
  (ok-sh "chmod" "u-w" filepath))

(defn file-dev-inode [filepath]
  (let [out (ok-sh "stat" filepath)
        [dev inode] (clojure.string/split out #" ")]
    (list (read-string dev) (read-string inode))))

(defn hash-str [text]
  (let [full-text (str "blob " (count text) "\0" text)]
    (sha-1-hex full-text)))

(defn git-root [] "./.git/")

(defn get-author [] "j <jamie.williams89@gmail.com>")
(def  get-committer get-author)

(defn read-file [path]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream path) out)
    (.toByteArray out)))

(defn decompress-zlib [data]
  (let [buffer (byte-array (alength data))
        out (java.io.ByteArrayOutputStream.)
        in (java.util.zip.InflaterInputStream. (java.io.ByteArrayInputStream. data)) ]
    (loop [bytes-read 0]
      (when-not (< bytes-read 0)
        (.write out buffer 0 bytes-read)
        (recur (.read in buffer))))
    (.flush out)
    (.close out)
    (.toByteArray out)))

(defn compress-zlib [data]
  (let [buffer (byte-array (alength data))
        out (java.io.ByteArrayOutputStream.)
        in (java.util.zip.DeflaterInputStream. (java.io.ByteArrayInputStream. data)) ]
    (loop [bytes-read 0]
      (when-not (< bytes-read 0)
        (.write out buffer 0 bytes-read)
        (recur (.read in buffer))))
    (.flush out)
    (.close out)
    (.toByteArray out)))

(defn write-object [bs]
  (let [hash-text (sha-1-hex bs)
        [d-name f-name] (map #(apply str %) (split-at 2 hash-text))
        filepath (str (git-root) "objects/" d-name "/" f-name)
        data (byte-array (map byte bs))
        bdata (compress-zlib data)]
    (make-parents filepath)
    (if (not (.exists (clojure.java.io/as-file filepath)))
      (do
        (with-open [wrtr (output-stream filepath)]
          (.write wrtr bdata))
        (unset-writable filepath)))
    filepath))

(defn write-blob [text]
  (write-object (str "blob " (count text) "\0" text)))

(defn write-commit [text]
  (write-object (str "commit " (count text) "\0" text)))

(defn hash-file [filepath]
  (hash-str (slurp filepath)))

;TODO: Use flags from entry hash-map
(defn tree-entry-bytes [entry]
  (let [flags-bs (if (= (:type entry) "tree")
                        '(52 48 48 48 48)
                        '(49 48 48 54 52 52))
        name-bs (->> (:name entry)
                     (map byte)
                     (byte-array)
                     (seq))
        hash-bs (->> (:hash entry)
                    (partition 2)
                    (map #(apply str %))
                    (map #(str "0x" %))
                    (map read-string)
                    (byte-array)
                    (seq))]
    (concat flags-bs
            '(32)
            name-bs
            '(00)
            hash-bs)))

(defn tree-data [tree-struct]
  (let [sorted (sort-by #(:name %) compare tree-struct)
        body (apply concat (map tree-entry-bytes sorted))
        size-bs (map byte (str (count body)))
        tree-str-bs (map byte "tree ")
        header-bs (concat tree-str-bs size-bs '(0))
        data (concat header-bs body)
        ]
    data))

(defn write-tree [tree-struct]
  (-> tree-struct
      (tree-data)
      (byte-array)
      (write-object)))

(defn hash-tree [tree-struct]
  (->> tree-struct
       (tree-data)
       (byte-array)
       (sha-1-hex)))

(defn all-objects []
  (let [object-path (str (git-root) "objects/")
        pref-files (drop 1 (file-seq (clojure.java.io/file object-path)))
        are-files (filter #(.isFile %) pref-files)
        hashes (map #(str (.getName (.getParentFile %))
                         (.getName %)) are-files)]
        
    (map #(do (assert (= (count %) SHA-1-HEX-LENGTH)) %) hashes)))

(defn complete-hash [hash-prefix]
  (assert (<= (count hash-prefix) SHA-1-HEX-LENGTH))
  (let [len (count hash-prefix)
        all (all-objects)
        ps (map #(subs % 0 len) all)
        pairs (map list ps all)
        matches (filter (fn [[x _]] (= x hash-prefix)) pairs)]
    (cond
      (empty? matches)
      (throw (Exception. 
               (str "No matching objects for prefix '" hash-prefix "'")))
      (> (count matches) 1)
      (throw (Exception. 
               (str "Ambiguous object prefix '" hash-prefix "'")))
      :else (second (first matches)))))

(defn read-object [hash-prefix]
  (let [hash-hex (complete-hash hash-prefix)
        filepath (str (git-root) 
                      "objects/"
                      (subs hash-hex 0 2) "/" 
                      (subs hash-hex 2 (count hash-hex)))
        bs (seq (decompress-zlib (read-file filepath)))
        ix (inc (.indexOf bs (byte 0)))]
        (assert (> ix 0))
        (split-at ix bs)))

(defn object-type [hash-hex]
  (let [header (to-str (first (read-object hash-hex)))]
    (first (clojure.string/split header #" "))))

(defn tree [h]
  (let [[header ins] (read-object h)
        f-entry (fn [bs]
                  (let [spacex (.indexOf bs (int \space))
                        flags (take spacex bs)
                        nxt (drop (inc spacex) bs)
                        nullx (.indexOf nxt 0)
                        name (take nullx nxt)
                        remain (drop (inc nullx) nxt)
                        len (/ SHA-1-HEX-LENGTH 2)
                        hash-vals (take len remain)
                        nxt-entry (drop len remain)
                        to-hex #(format "%02x" (mod % 256))
                        hash-str (clojure.string/join
                                   (map to-hex hash-vals))
                        obj-type (object-type hash-str)]
                    (list { :name (to-str name)
                            :flags (to-str flags)
                            :type obj-type
                            :hash hash-str}  nxt-entry)))]
    (assert (= "tree" (apply str (take 4 (map char header)))))
    (loop [[e r] (f-entry ins) out []]
      (if (empty? r)
        (conj out e)
        (recur (f-entry r) (conj out e))))))

(defn blob [hash-hex]
  (to-str (second (read-object hash-hex))))

(def commit blob)

(defn branch [b]
  (let [filepath (str (git-root) "refs/heads/" b)]
    (clojure.string/replace (slurp filepath) #"\n" "")))

(defn head []
  (let [filepath (str (git-root) "HEAD")
        full-branch (clojure.string/replace (slurp filepath) #"\n" "")
        branch-name (-> full-branch (clojure.string/split #"\/") (last))]
    (branch branch-name)))

(defn validarte-index [] nil)

;TODO Check validation bits to ensure correct parsing

(defn read-index []
  (let [to-num (fn [x] (->> x
                            (clojure.string/join)
                            (str "0x")
                            (read-string)))
        filepath (str (git-root) "index")
        xs (read-file filepath)
        hs (vec (map #(format "%02x" (mod % 256)) xs))
        header-len 12
        dirc (to-str (subvec (vec xs) 0 4))
        version (to-num (subvec hs 4 8))
        file-count (to-num (subvec hs 8 12))
        f-entry (fn [bs]
          (let [vbs (vec bs)
                name-len (mod (to-num (subvec vbs 0x3C 0x3E)) 4096)
                name-bs (subvec vbs 0x3E (+ 0x3E name-len))
                entry-len (+ 62 name-len)
                zero-count (- 8 (mod entry-len 8))
                ctime-s (to-num (subvec vbs 0x00 0x04))
                ctime-ns (to-num (subvec vbs 0x04 0x08))
                mtime-s (to-num (subvec vbs 0x08 0x0C))
                mtime-ns (to-num (subvec vbs 0x0C 0x10))]
            (list (drop (+ entry-len zero-count) bs)
                  {:inode (to-num (subvec vbs 0x14 0x18)),
                   :device (to-num (subvec vbs 0x10 0x14)),
                   :filesize (to-num (subvec vbs 0x24 0x28)),
                   :name (to-str (map to-num name-bs)),
                   :ctime (java.util.Date. (+ (* 1000 ctime-s) 
                                              (/ ctime-ns 1000)))
                   :mtime (java.util.Date. (+ (* 1000 mtime-s) 
                                              (/ mtime-ns 1000)))
                   :hash (clojure.string/join (subvec vbs 0x28 0x3C))})))]

    (assert (= dirc "DIRC"))
    (assert (= version 2))
    (loop [[r e] (f-entry (drop header-len hs))
           out []]
      (if (= (count out) (dec file-count))
        (conj out e)
        (recur (f-entry r) (conj out e))))))

(defn file-mtime [filepath]
  (java.util.Date. (.lastModified (java.io.File. filepath))))

(defn file-size [filepath]
  (.length (clojure.java.io/file filepath)))

; TODO: If times differ, compute hash to check for differences
;   if not really different, update modified times back to index..?!

; TODO: If times match, also check filesize

; TODO: If times differ, double-check size then hash..
(defn modified []
  (->> (read-index)
       (map #(list (:name %) (:mtime %) (file-mtime (:name %))))
       (filter #(not= 0 (.compareTo (nth % 1) (nth % 2))))
       (map first)))

(defn files-to-commit []) ;TODO; This

(defn file-index-entry [fp]
  (let [[device inode] (file-dev-inode fp)]
    {:inode inode,
     :device device,
     :filesize (file-size fp),
     :name fp,
     :ctime (file-mtime fp),
     :mtime (file-mtime fp),
     :hash (hash-file fp)}))

(defn positions [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(defn build-file-tree [file-entry]
  (let [filepath (:name file-entry)
        indexes (positions #(= \/ %) filepath)
        dirs (map #(apply str (take % filepath)) indexes)
        f-parent (fn [x] (second (re-matches #"(.*)/[^/]+" x)))
        dirs-map (reduce #(assoc %1 %2
            {:type :dir, :parent (f-parent %2)}) {} dirs)]
    (assoc dirs-map filepath
           (dissoc
             (into file-entry {:type :file, :parent (f-parent filepath)})
             :name))))

(defn files-tree [file-entries]
  (let [objs (reduce #(into %1 %2) {} (map build-file-tree file-entries))
        find-children (fn [p]
                        (->> objs
                             (filter #(= p (:parent (second %))))
                             (map first)))]
    (reduce (fn [x y]
           (assoc x y (assoc
                        (get x y)
                        :children
                        (find-children y)))
           ) objs (keys objs))))

(defn files-map-to-objs [files-map]
  (let [named-list (reduce (fn [coll k]
                             (conj coll
                                   (assoc (get files-map k)
                                          :fname k))) [] (keys files-map))
        with-dept (map (fn [x]
                         (assoc x :dept
                                (count (filter #(= \/ %) (:fname x)))))
                       named-list)
        ordered (reverse (sort-by #(:dept %) with-dept))
        with-names (map (fn [x]
                          (assoc x :name 
                                 (last (clojure.string/split
                                        (:fname x) #"\/"))))
                        ordered)
        blob-objs (->> with-names
                       (filter #(= (:type %) :file))
                       (map #(-> %
                                 (assoc :type "blob")
                                 (assoc :flags "100644"))))
        dir-objs (->> with-names
                       (filter #(= (:type %) :dir))
                       (map #(-> %
                                 (assoc :type "tree")
                                 (assoc :flags "40000"))))
        f-children (fn [ds x]
                     (map #(select-keys % [:name
                                           :type
                                           :hash
                                           :flags])
                          (filter #(= (:parent %) (:fname x))
                                  (concat blob-objs ds))))
        with-hashes (reduce (fn [objs x]
                          (conj objs
                             (assoc x :hash
                                    (hash-tree (f-children objs x)))
                             )) [] dir-objs)
        tree-objs (map (fn [x]
                         {:object-type :tree
                          :contents (f-children with-hashes x)}) 
                       with-hashes)
        top-children (f-children with-hashes nil)
        top-tree-obj {:object-type :tree
                      :contents top-children}]
    (list (hash-tree top-children) (conj tree-objs top-tree-obj))))

(defn index-to-tree-objects []
  (let [index-files (read-index)
        files-map (files-tree index-files)
        [top-hash objs] (files-map-to-objs files-map)]
    [top-hash objs]))

; Creates a new commit object (and all required tree objects)
; from the current index
; TODO: Update the index, HEAD and branch after creating objects!
(defn create-commit [message parent-hash]
  (let [[tree-hash tree-structs] (index-to-tree-objects)
        timestamp (str (current-unix-time) " +0000") ; TODO: Use current timezone
        text (str "tree " tree-hash "\n"
                  "parent " parent-hash "\n"
                  "author " (get-author) " " timestamp "\n"
                  "committer " (get-committer) " " timestamp "\n"
                  "\n"
                  message "\n")]
    (doseq [e tree-structs] (write-tree (:contents e)))
    (write-commit text)))

