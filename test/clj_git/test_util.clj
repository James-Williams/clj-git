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
      (list 'with-redefs ['clj-git.repo/repo-root (list 'fn [] (str sandbox-pathname "/"))]
        (list 'let (vector 'err '(atom nil))
          (list 'clojure.java.shell/with-sh-dir (list 'clj-git.repo/repo-root)
            (cons 'do (concat
              (list (list 'ok-sh "git" "checkout" "."))
              (list (list 'try
                (cons 'do body)
                (list 'catch 'Exception 'e '(reset! err e))
                (list 'finally
                  (list 'ok-sh "rm" "-rf" (str "../" sandbox-pathname))
                  '(if @err (throw @err))))))))))))

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

(deftest t-with-repo-sandbox-cleanup
  (testing "with-repo-sandbox cleanups up dir after exception"
    (is (thrown-with-msg? Exception #"throw from test"
      (with-repo-sandbox "fixtures/base_repo.git" "t-with-repo-sandbox-cleanup"
        (throw (Exception. "throw from test")))))
    (is (not (.exists (clojure.java.io/as-file "t-with-repo-sandbox-cleanup"))))
  )
)

(deftest t-with-repo-sandbox-multiargs
  (testing "with-repo-sandbox handles mutiple body args"
    (with-repo-sandbox "fixtures/base_repo.git" "t-with-repo-sandbox-multiargs"
      (+ 1 2) (- 3 4)
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
