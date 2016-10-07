(ns clj-git.core
  (:use clj-git.util)
  (:use clj-git.repo)
  (:use clj-git.file)
  (:use clj-git.object)
  (:use clj-git.index)
  (:use clj-git.tree)
  (:gen-class))

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
