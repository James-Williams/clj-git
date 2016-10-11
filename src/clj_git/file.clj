(ns clj-git.file
  (:use clj-git.util)
  (:use clj-git.repo)
  (:gen-class))

(defn file-dev-inode [filepath]
  (let [out (ok-sh "stat" filepath)
        [dev inode] (clojure.string/split out #" ")]
    (list (read-string dev) (read-string inode))))

(defn file-mtime [filepath]
  (java.util.Date. (.lastModified (java.io.File. (str (repo-root) filepath)))))

(defn file-size [filepath]
  (.length (clojure.java.io/file (str (repo-root) filepath))))
