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
  (testing "Check structure for multi-line commit"
    (is (=
      (commit "a4c719416bd67212ec386b2bbb3405c62b0c8ac5")
      {:message
      "Don't write objects that exist.\n\nSet files to non-writable once written",
      :tree "de850342386653cd371fd3a0eb2c0d7e051052e6",
      :parent "7d6811bc2e5065e170a5b6d787283c7054927f32",
      :author "j <jamie.williams89@gmail.com> 1453647595 +0000",
      :committer "j <jamie.williams89@gmail.com> 1453647595 +0000"}
    )))
)
