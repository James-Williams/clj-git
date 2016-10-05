(ns clj-git.tree-test
  (:require [clojure.test :refer :all]
            [clj-git.tree :refer :all]))

(deftest t-tree-to-files
  (testing "Single file returned for first commit tree"
    (is (=
          (tree-to-files "ca93b49848670d03b3968c8a481eca55f5fb2150")
          [{:name "test_file", :hash "9daeafb9864cf43055ae93beb0afd6c7d144bfa4"}]
    )))
  (testing "Files from different paths returned for second commit tree"
    (is (=
          (tree-to-files "0b316f5f72777bb277321f6a497f4843d79ec6ed")
          [ {:name ".gitignore", :hash "c53038ec0e3d3094216b6a0fed8967e71cad6f5d"}
            {:name "CHANGELOG.md",
            :hash "5eb5adc5251ce0e5cd457014543a6de4d519e178"}
            {:name "LICENSE", :hash "7689f30efd6dcaadfb4033a444fa1c3f1848dba3"}
            {:name "README.md", :hash "b45732281a0577db19bfb5c922322cef26de3943"}
            {:name "doc/intro.md",
            :hash "6c6223bb5a443a35a2740c1c535bd9aa7a6583fb"}
            {:name "project.clj",
            :hash "309d2915d3d229fc462a87ea0f43db8e86c9628d"}
            {:name "src/clj_git/.core.clj.swp",
            :hash "f489403772cb9458b052a79a0882618867d50106"}
            {:name "src/clj_git/core.clj",
            :hash "31608502b72813f01e448f26df84530c3c0a07af"}
            {:name "test/clj_git/.core_test.clj.swp",
            :hash "d30d8770e5eec69aa5a1a06bea66f493388dcd80"}
            {:name "test/clj_git/core_test.clj",
            :hash "a99b98dd69748ae2c71853754ab5ab0f9cb4f263"}
            {:name "test_file", :hash "9daeafb9864cf43055ae93beb0afd6c7d144bfa4"}]
    )))
)
