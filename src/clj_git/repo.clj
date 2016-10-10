(ns clj-git.repo
  (:use clojure.java.io)
  (:use clj-git.util)
  (:gen-class))

(defn repo-root [] "./")

(defn git-root [] (str (repo-root) ".git/"))

; TODO: Read this properly..
(defn get-author [] "j <jamie.williams89@gmail.com>")
(def  get-committer get-author)

(defn branch [b]
  (let [filepath (str (git-root) "refs/heads/" b)]
    (clojure.string/replace (slurp filepath) #"\n" "")))

(defn head-name []
  (let [filepath (str (git-root) "HEAD")
        full-branch (clojure.string/replace (slurp filepath) #"\n" "")
        branch-name (-> full-branch (clojure.string/split #"\/") (last))]
  branch-name))

(defn head []
  (branch (head-name)))

(defn move-branch [branch-name full-hash]
  (let [filepath (str (git-root) "refs/heads/" branch-name)]
    (assert (.exists (as-file filepath)))
    (spit filepath full-hash)))
