(ns clj-git.tree
  (:use clj-message-digest.core)
  (:use clj-git.object)
  (:gen-class))

(defn tree-to-files
  ( [tree-hash] (tree-to-files tree-hash "") )
  ( [tree-hash path-prefix]
    (->> (tree tree-hash)
      (map  (fn [entry]
              (case (:type entry)
                "blob"  [ { :name (str path-prefix (:name entry))
                          , :hash (:hash entry)}]
                "tree"  (tree-to-files
                          (:hash entry)
                          (str path-prefix (:name entry) "/")
              ))))
      (apply concat)
    ))
)
