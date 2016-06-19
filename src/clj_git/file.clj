(ns clj-git.file
  (:use clj-git.util)
  (:gen-class))

(defn file-dev-inode [filepath]
  (let [out (ok-sh "stat" filepath)
        [dev inode] (clojure.string/split out #" ")]
    (list (read-string dev) (read-string inode))))

(defn file-mtime [filepath]
  (java.util.Date. (.lastModified (java.io.File. filepath))))

(defn file-size [filepath]
  (.length (clojure.java.io/file filepath)))
