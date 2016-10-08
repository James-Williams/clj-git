(ns clj-git.index
  (:use clj-message-digest.core)
  (:use clojure.java.io)
  (:require [clojure.set :only [difference]])
  (:use clj-git.util)
  (:use clj-git.repo)
  (:use clj-git.file)
  (:use clj-git.object)
  (:use clj-git.tree)
  (:gen-class))

; TOOD: Get this to return a simple [int] type
;   -> Move all functions to simpler types..
(defn build-index [index-struct]
  (let [out                 (java.io.ByteArrayOutputStream.)
        wstr                (fn [s] (.write out (.getBytes s)))
        wchars              (fn [cs] (doseq [c cs] (.write out (int c))))
        file-count          (count index-struct)
        sorted-index-struct (sort-by #(:name %) index-struct)]
    (wstr "DIRC")
    (wchars [0 0 0 2])
    (wchars (int-bytes file-count 4))
    (doseq [entry sorted-index-struct]
      (let [ctime-s   (/ (.getTime    (:ctime entry)) 1000)
            ctime-ns  (mod (.getTime  (:ctime entry)) 1000)
            mtime-s   (/ (.getTime    (:mtime entry)) 1000)
            mtime-ns  (mod (.getTime  (:mtime entry)) 1000)
            bs (concat
                (int-bytes ctime-s 4)
                (int-bytes ctime-ns 4)
                (int-bytes mtime-s 4)
                (int-bytes mtime-ns 4)
                (int-bytes (:device entry) 4)
                (int-bytes (:inode entry) 4)
                [0 0 129 164]  ; TODO: What is this??
                [0 0 1 245]    ; UID TODO: Record in struct
                [0 0 0 20]     ; GID TODO: Record in struct
                (int-bytes (:filesize entry) 4)
                (hex-bytes (:hash entry))
                [0] ; Flags : TODO: Find out about this
                [(count (:name entry))]
                (map int (:name entry))
               )
            bs-padded (concat bs (repeat (- 8 (mod (count bs) 8)) 0))]
            (wchars bs-padded)
          ))
    (wchars (hex-bytes (sha-1-hex (byte-array (.toByteArray out)))))
    out))

(defn write-index
  ( [index-struct] (write-index index-struct (str (git-root) "index")))
  ( [index-struct filepath]
    (with-open [wrtr (output-stream filepath)]
      (doseq [b (byte-seq (build-index index-struct))]
        (.write wrtr b)))))

;TODO Check validation bits to ensure correct parsing
(defn read-index
  ( [] (read-index (-> (str (git-root) "index") read-file )))
  ( [stream]
    (let [to-num (fn [x] (->> x
                              (clojure.string/join)
                              (str "0x")
                              (read-string)))
          byte-array (.toByteArray stream)
          hs (vec (map #(format "%02x" (mod % 256)) byte-array))
          header-len 12
          dirc (to-str (subvec (vec byte-array) 0 4))
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
          (recur (f-entry r) (conj out e)))))))

(defn file-index-entry [filename]
  (let [[device inode] (file-dev-inode filename)]
    {:inode inode,
     :device device,
     :filesize (file-size filename),
     :name filename,
     :ctime (file-mtime filename),
     :mtime (file-mtime filename),
     :hash (hash-file filename)}))

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

(defn stage-file [filename]
  (let [new-entry (file-index-entry filename)]
    (write-file-blob filename)
    (->> (read-index)
      (filter #(not= (:name %) filename))
      (cons new-entry)
      (write-index))
))

; Use the following rules to quickly check for modified files:
;         if filesystem mtime matches index           -> return False
;   else  if filesystem filesize differs from index   -> return True
;   else  check hash from filesystem vs index         -> return hash comparison
; NOTE: Do not check mtime and filesize when index inode == 0..
(defn is-file-modified
  ( [filename]  (is-file-modified
                  filename
                  (->> (read-index) (filter #(= (:name %) filename)) first)) )
  ( [filename index-entry]
    (let [good-index  (not= (:inode index-entry) 0)] ; TODO: Lookup why this is needed
      (cond
        (and good-index (= 0 (.compareTo (:mtime index-entry) (file-mtime filename))))  false
        (and good-index (not= (:filesize index-entry) (file-size filename)))            true
        :else                 (not= (:hash index-entry) (hash-file filename))
    ))))

(defn list-modified-files []
  (let [index       (read-index)
        index-names (map #(:name %) index)
        index-map   (into {} (map vector index-names index))]
    (->> index-names (filter
      (fn [filename]
        (is-file-modified filename (get index-map filename)))))))

(defn list-staged-files []
  (let [index       (read-index)
        index-names (map #(:name %) index)
        index-map   (into {} (map vector index-names index))
        head-files  (tree-to-files (:tree (commit (head))))
        head-names  (map #(:name %) head-files)
        head-map    (into {} (map vector head-names head-files))]
    (->> index-names (filter
      (fn [filename]
        (let [hash-from-index (:hash (get index-map filename))
              hash-from-head  (:hash (get head-map filename))]
          (not= hash-from-head hash-from-index)))))))

(defn is-file-staged [filename]
  (contains? (into #{} (list-staged-files)) filename))

(defn list-all-files []
  (->>
    (clojure.java.io/file ".")
    (file-seq)
    (filter #(not (.isDirectory %)))
    (map #(.getPath %))
    (filter #(not (glob-match? "/.git/" %)))
))

(defn list-files []
  (let [gitignore-patterns  (-> (slurp ".gitignore") (clojure.string/split #"\n"))
        buildin-patterns    [".gitignore"]
        patterns            (concat buildin-patterns gitignore-patterns)
        f                   (fn [g ss] (filter #(not (glob-match? g %)) ss))
        filters             (map #(partial f %) patterns)]
    (->> (list-all-files)
      ((apply comp filters))
      (map #(.substring % 2))
  )
))

(defn list-untracked-files []
  (let [index       (read-index)
        index-names (map #(:name %) index)
        index-set   (into #{} index-names)
        wd-set      (into #{} (list-files))]
    (vec (clojure.set/difference wd-set index-set))))
