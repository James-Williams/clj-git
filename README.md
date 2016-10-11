# clj-git

An implementation of Git Version Control in Clojure.

A library of Clojure functions providing a subset of Git functionality. This includes the manipulation of the working tree, the index and the object store.

DISCLAIMER: This project has been developed for fun and was initially intended just to be a vehicle for learning Clojure. It's a work-in-progress and is not suitable for serious use in it's current state.

## Examples
FIXME: Use the machine-readable version of (print-status) in the examples

### Commit a modified file
```clojure
(spit "test_file" "New Line!\n" :append true)

(print-status)
Unstaged Changes:
  test_file
  
(stage-file "test_file")

(print-status)
Staged For Commit:
  test_file
  
(create-commit "Added a new line to 'test_file'")
"<NEW_SHA1_HASH>"

(print-status)
Nothing to report, working directory clean
```

### Restore a file
```clojure
(slurp "test_file")
"test\n"

(clojure.java.io/delete-file "test_file")

(slurp "test_file")
FileNotFoundException test_file (No such file or directory) ..

(print-status)
Unstaged Changes:
  test_file

(checkout-file "test_file")

(print-status)
Nothing to report, working directory clean

(slurp "test_file")
"test\n"
```

### List working files
```clojure
(list-files)
["a_dir/existing_file"]

(list-untracked-files)
[]

(list-modified-files)
[]

(spit "new_file" "I'm a brand new file!\n")

(spit "a_dir/existing_file" "New Contents!\n")

(list-files)
["a_dir/existing_file" "new_file"]

(list-untracked-files)
["new_file"]

(list-modified-files)
["a_dir/existing_file"]
```

## Feature Status

Feature | Status
------- | ------
Read/Write Objects | Done
Read/Write Index | Working (no support for file permissions)
Read/Write Working Tree | Working (better support for CWD required)
Add Files / Reset Files | Done
Create Commits | Done
Pull / Push | Todo
Packfile Support | Todo
Branch Support | Todo
File Permissions | Todo
Initalize New Repo | Todo
View History | Todo
Merging / Rebasing | Todo
Diffing | Todo
File-Merging | Todo
Config / User IDs | Todo
Hook Scripts | Todo
Logging | Todo
Porcelain Commands | Todo

## Testing

Development follows the Test Driven Development pattern of RED-GREEN-REFACTOR.

Run all UNIT and INTEGRATION tests: `lein test`

ISSUE: Integration tests currently rely on the projects own Git repository as a text fixture. This has resulted in issues with test reproducibility, independance and stability. Going forwards, existing tests will be ported to use a seperate Git repository sandbox.

## License

Copyright © 2016

Distributed under the Eclipse Public License either version 1.0
