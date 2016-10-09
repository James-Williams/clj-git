(ns clj-git.object-test
  (:require [clojure.test :refer :all]
            [clj-git.object :refer :all]))

(deftest t-commit
  (testing "Check structure for first commit"
    (is (=
      (commit "bfac6b45a0b4086a308a1d1deac59fef612917f8")
      { :message "Test file added perminantly",
        :tree "ca93b49848670d03b3968c8a481eca55f5fb2150",
        :parents [],
        :author "j <jamie.williams89@gmail.com> 1453468511 +0000",
        :committer "j <jamie.williams89@gmail.com> 1453468511 +0000"} 
    )))
  (testing "Check structure for multi-line commit"
    (is (=
      (commit "a4c719416bd67212ec386b2bbb3405c62b0c8ac5")
      {:message
      "Don't write objects that exist.\n\nSet files to non-writable once written",
      :tree "de850342386653cd371fd3a0eb2c0d7e051052e6",
      :parents ["7d6811bc2e5065e170a5b6d787283c7054927f32"],
      :author "j <jamie.williams89@gmail.com> 1453647595 +0000",
      :committer "j <jamie.williams89@gmail.com> 1453647595 +0000"}
    )))
  (testing "Commit will multiple parents"
    (is (= (commit "1d8678f71731f37086ad2c91d11b0bd092e8b5a9")
          {:message
          "Merge branch 'parent' to create a multi-parent commit (for use in testing)",
          :parents  [ "96c9883dcb0c0acbed226cb4b43ac6ce4eefb683"
                      "db96d077657a33f4b637d0e89cd68cdbdb1cdfd4" ]
          :tree "d250a204642f650a4a3858f3b817b0390c07719f",
          :author "James Williams <j@Jamess-MacBook-2.local> 1476027126 +0100",
          :committer
          "James Williams <j@Jamess-MacBook-2.local> 1476027126 +0100"}))
  )
)

(deftest t-write-blob
  (testing "Store test_file blob")
    (is (= (write-blob (slurp "test_file"))
           "9daeafb9864cf43055ae93beb0afd6c7d144bfa4")))

(deftest t-write-file-blob
  (testing "Store test_file blob")
    (is (= (write-file-blob "test_file")
           "9daeafb9864cf43055ae93beb0afd6c7d144bfa4")))
