(ns clj-git.repo
  (:gen-class))

(defn git-root [] "./.git/")

(defn get-author [] "j <jamie.williams89@gmail.com>")
(def  get-committer get-author)

(defn branch [b]
  (let [filepath (str (git-root) "refs/heads/" b)]
    (clojure.string/replace (slurp filepath) #"\n" "")))

(defn head []
  (let [filepath (str (git-root) "HEAD")
        full-branch (clojure.string/replace (slurp filepath) #"\n" "")
        branch-name (-> full-branch (clojure.string/split #"\/") (last))]
    (branch branch-name)))

