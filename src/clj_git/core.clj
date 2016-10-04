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

; TODO: If times differ, compute hash to check for differences
;   if not really different, update modified times back to index..?!
; TODO: If times match, also check filesize
; TODO: If times differ, double-check size then hash..
(defn modified []
  (->> (read-index)
       (map #(list (:name %) (:mtime %) (file-mtime (:name %))))
       (filter #(not= 0 (.compareTo (nth % 1) (nth % 2))))
       (map first)))

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
