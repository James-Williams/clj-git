(ns clj-git.object-test
  (:require [clojure.test :refer :all]
            [clj-git.object :refer :all]))

(deftest t-commit
  (testing "Check structure for first commit"
    (is (=
      (commit "bfac6b45a0b4086a308a1d1deac59fef612917f8")
      { :message "Test file added perminantly",
        :tree "ca93b49848670d03b3968c8a481eca55f5fb2150",
        :author "j <jamie.williams89@gmail.com> 1453468511 +0000",
        :committer "j <jamie.williams89@gmail.com> 1453468511 +0000"} 
    )))
)
