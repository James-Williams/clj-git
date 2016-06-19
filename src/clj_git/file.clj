(ns clj-git.file
  (:use clojure.java.io)
  (:use clojure.java.shell)
  (:use clj-git.util)
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn file-dev-inode [filepath]
  (let [out (ok-sh "stat" filepath)
        [dev inode] (clojure.string/split out #" ")]
    (list (read-string dev) (read-string inode))))

(defn file-mtime [filepath]
  (java.util.Date. (.lastModified (java.io.File. filepath))))

(defn file-size [filepath]
  (.length (clojure.java.io/file filepath)))
