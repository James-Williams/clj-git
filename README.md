# clj-git

FIXME: description

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

## License

Copyright © 2016

Distributed under the Eclipse Public License either version 1.0
