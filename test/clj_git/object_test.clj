(ns clj-git.object-test
  (:require [clojure.test :refer :all]
            [clj-git.test-util :refer :all]
            [clj-git.util :refer :all]
            [clj-git.repo :refer :all]
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
  (with-repo-sandbox "fixtures/base_repo.git" "t-write-blob"
    (testing "Store test_file blob")
      (is (= (write-blob (.getBytes (slurp (str (repo-root) "test_file"))))
             "9daeafb9864cf43055ae93beb0afd6c7d144bfa4"))
))

(deftest t-write-file-blob
  (with-repo-sandbox "fixtures/base_repo.git" "t-write-file-blob"
    (testing "Store test_file blob")
      (is (= (write-file-blob "test_file")
             "9daeafb9864cf43055ae93beb0afd6c7d144bfa4"))
))

(deftest t-hash-blob
  (testing "Empty Blob"
    (is (= (hash-blob [])
            "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"))
  )
  (testing "Test Blob"
    (is (= (hash-blob (.getBytes "test\n"))
            "9daeafb9864cf43055ae93beb0afd6c7d144bfa4"))
  )
)

(deftest t-hash-file
  (with-repo-sandbox "fixtures/base_repo.git" "t-hash-file"
    (testing "Hash test_file")
      (is (= (hash-file "test_file")
             "9daeafb9864cf43055ae93beb0afd6c7d144bfa4")
    )
    (testing "Hash unicode char"
      (spit (str (repo-root) "utf8_file") "©\n")
      (is (= (hash-file "utf8_file")
             "767079b3f717d1ffad2746326a624a2bea694ad5"))
    )
    (testing "Hash binary file"
      (with-open [wr (clojure.java.io/output-stream (str (repo-root) "binfile"))]
        (.write wr (byte-array [0xb9 0x90 0x8d 0x7f 0x28 0x5e 0x22 0x81 0x0a])))
      (is (= (hash-file "binfile")
             "e3f68abb8545e2fa184b835c5e3feb65476833d6"))
    )
  )
)

(deftest t-complete-hash
  (testing "Complete '9daead'"
    (is (= (complete-hash "9daeaf")
            "9daeafb9864cf43055ae93beb0afd6c7d144bfa4"))))

(deftest t-branch-lookup
  (with-repo-sandbox "fixtures/base_repo.git" "t-branch-lookup"
    (testing "Lookup branch 'first'"
      (is (= (branch "first")
              "844f34ae464bb93210bff3f933e21f5585cd4d6d")))))

(deftest t-tree
  (testing "Two entry tree for ./test/clj-git/"
    (is (= (tree "47254fa494e4ab6556b5f434fb2575d0e0c3ff17")
            [ {:type "blob" :name "core_test.clj" :flags "100644"
               :hash "9348f6141516da61950a85f844d858f172475e82"}
              {:type "blob" :name "temp" :flags "100644"
               :hash "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"}])))
  )

(deftest t-hash-tree
  (testing "Check that trees hash to original values")
    (is (= (hash-tree (tree "6811239b02050711578e223ffe1c297a0eb801f4"))
           "6811239b02050711578e223ffe1c297a0eb801f4"))
  (testing "Top level tree entry with both blob and tree children")
    (is (= (hash-tree (tree "4b90f754d0ddf46643fb5250b47cddfb43102f30"))
           "4b90f754d0ddf46643fb5250b47cddfb43102f30"))
  (testing "Reversing object list has no effect")
    (is (= (hash-tree (reverse
                        (tree "4b90f754d0ddf46643fb5250b47cddfb43102f30")))
           "4b90f754d0ddf46643fb5250b47cddfb43102f30"))
  (testing "Check that trees hash to original values")
    (is (= (hash-tree (tree "ca93"))
           "ca93b49848670d03b3968c8a481eca55f5fb2150"))
)

