(ns clj-git.core
  (:use clj-git.util)
  (:use clj-git.repo)
  (:use clj-git.file)
  (:use clj-git.object)
  (:use clj-git.index)
  (:use clj-git.tree)
  (:gen-class))

(defn print-status []
  (let [modified  (list-modified-files)
        staged    (list-staged-files)
        untracked (list-untracked-files)]
  (if-not (empty? staged)
    (do
      (println "\nStaged For Commit:")
      (doseq [x staged]
        (println (str "  " x)))))

  (if-not (empty? modified)
    (do
      (println "\nUnstaged Changes:")
      (doseq [x modified]
        (println (str "  " x)))))

  (if-not (empty? untracked)
    (do
      (println "\nUntracked Files:")
      (doseq [x untracked]
        (println (str "  " x)))))
  (if (every? empty? [staged modified untracked])
    (println "Nothing to report, working directory clean"))
))

(defn -main [& args] (print-status))

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

(defn stage-all-changes []
  (doseq [filename (list-modified-files)]
    (stage-file filename))
  (print-status))
