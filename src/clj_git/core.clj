(ns clj-git.core
  (:use clj-message-digest.core)
  (:use clojure.java.io)
  (:use clojure.java.shell)
  (:use clj-git.util)
  (:use clj-git.repo)
  (:use clj-git.object)
  (:require [clojure.java.io :as io])
  (:gen-class))

(declare modified)
(defn -main [& args]
  (println "Unstaged Changes:")
  (doseq [x (modified)]
    (println (str "  " x))))

(defn current-unix-time []
  (quot (System/currentTimeMillis) 1000))

(defn file-dev-inode [filepath]
  (let [out (ok-sh "stat" filepath)
        [dev inode] (clojure.string/split out #" ")]
    (list (read-string dev) (read-string inode))))

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

