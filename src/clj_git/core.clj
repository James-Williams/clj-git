(ns clj-git.core
  (:use clj-git.util)
  (:use clj-git.repo)
  (:use clj-git.file)
  (:use clj-git.object)
  (:use clj-git.index)
  (:gen-class))

(declare modified)
(defn -main [& args]
  (println "Unstaged Changes:")
  (doseq [x (modified)]
    (println (str "  " x))))

; Use the following rules to quickly check for modified files:
;         if filesystem mtime matches index           -> return False
;   else  if filesystem size differs from index       -> return True
;   else  check hash from filesystem vs index         -> return hash comparison
(defn is-file-modified [filename]
  (let [index-entry (->> (read-index) (filter #(= (:name %) filename)) first)]
    (cond
      (= 0 (.compareTo (:mtime index-entry) (file-mtime filename))) false
      (not= (:filesize index-entry) (file-size filename))           true
      :else                 (not= (:hash index-entry) (hash-file filename))
    )))

(defn modified []
  (->> (read-index)
       (map :name)
       (filter is-file-modified)))

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
