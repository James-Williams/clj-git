(ns clj-git.test-util
  (:require [clojure.test :refer :all]
            [clj-git.repo :refer :all]
            [clj-git.util :refer :all])
  (:gen-class))

(defmacro with-repo-sandbox [bare-repo-path sandbox-pathname & body]
  (list 'do
      (list 'assert (list '.isDirectory (list 'clojure.java.io/as-file bare-repo-path)))
      (list 'assert (list 'not (list '.exists (list 'clojure.java.io/as-file sandbox-pathname))))
      (list 'assert (list '.mkdir (list 'clojure.java.io/as-file sandbox-pathname)))
      (list 'ok-sh "cp" "-rf" bare-repo-path (str sandbox-pathname "/.git/"))
      (list 'with-redefs ['repo-root (list 'fn [] (str sandbox-pathname "/"))]
        (list 'clojure.java.shell/with-sh-dir (list 'repo-root)
          (list 'ok-sh "git" "checkout" ".")
          (list 'let ['res (cons 'do body)]
            (list 'ok-sh "rm" "-rf" (str "../" sandbox-pathname))
            'res)))))

(deftest t-with-repo-sandbox
  (with-repo-sandbox "fixtures/base_repo.git" "t-with-repo-sandbox"
    (testing ".git folder copied in correctly"
      (is (.isDirectory (clojure.java.io/as-file "t-with-repo-sandbox/.git")))
      (is (.exists (clojure.java.io/as-file "t-with-repo-sandbox/.git/index")))
    )
    (testing "(git-root) points to the correct location"
      (is (= (git-root) "t-with-repo-sandbox/.git/"))
    )
  )
)

(deftest t-bare-repo-sandbox
  (with-repo-sandbox "fixtures/base_repo.git" "t-bare-repo-sandbox"
    (testing "test_file correctly checked out"
      (is (.exists (clojure.java.io/as-file (str (repo-root) "test_file"))))
    )
  )
)