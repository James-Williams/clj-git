(ns clj-git.core
  (:use clj-git.util)
  (:use clj-git.repo)
  (:use clj-git.file)
  (:use clj-git.object)
  (:use clj-git.index)
  (:use clj-git.tree)
  (:gen-class))

(declare is-file-modified)

(defn -main [& args]
  (let [modified (->>  (read-index)
                       (map :name)
                       (filter is-file-modified))
        staged (->>  (read-index)
                       (map :name)
                       (filter is-file-staged))]
  (if-not (empty? modified)
    (do
      (println "Unstaged Changes:")
      (doseq [x modified]
        (println (str "  " x)))))

  (if-not (empty? staged)
    (do
      (println "Staged For Commit:")
      (doseq [x staged]
        (println (str "  " x)))))
))

; Use the following rules to quickly check for modified files:
;         if filesystem mtime matches index           -> return False
;   else  if filesystem filesize differs from index   -> return True
;   else  check hash from filesystem vs index         -> return hash comparison
; NOTE: Do not check mtime and filesize when index inode == 0..
(defn is-file-modified [filename]
  (let [index-entry (->> (read-index) (filter #(= (:name %) filename)) first)
        good-index  (not= (:inode index-entry) 0)] ; TODO: Lookup why this is needed
    (cond
      (and good-index (= 0 (.compareTo (:mtime index-entry) (file-mtime filename))))  false
      (and good-index (not= (:filesize index-entry) (file-size filename)))            true
      :else                 (not= (:hash index-entry) (hash-file filename))
    )))

; TODO: This is slow! (checking if any files are staged takes a long time..)
(defn is-file-staged [filename]
  (let [head-tree (tree-to-files (:tree (commit (head))))
        tree-entry      (->> head-tree (filter #(= (:name %) filename)) (first))
        index-entry     (->> (read-index) (filter #(= (:name %) filename)) (first))
        hash-from-head  (:hash tree-entry)
        hash-from-index (:hash index-entry)]
    (not= hash-from-head hash-from-index)))

; Creates a new commit object (and all required tree objects)
; from the current index
(defn create-commit
  ( [message]
    (create-commit message (head)))
  ( [message parent-hash]
    (let [[tree-hash tree-structs] (index-to-tree-objects)
          timestamp (str (current-unix-time) " +0000") ; TODO: Use current timezone
          text (str "tree " tree-hash "\n"
                    "parent " parent-hash "\n"
                    "author " (get-author) " " timestamp "\n"
                    "committer " (get-committer) " " timestamp "\n"
                    "\n"
                    message "\n")]
      (doseq [e tree-structs] (write-tree (:contents e)))
      (let [new-commit-hash (write-commit text)]
        (move-branch (head-name) new-commit-hash)
        new-commit-hash))))
