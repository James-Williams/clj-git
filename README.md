# clj-git

FIXME: description

## Examples
FIXME: Use the machine-readable version of (print-status) in the examples

### Modify a file and commit the change
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

### Restore a deleted file from the index
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

## License

Copyright © 2016

Distributed under the Eclipse Public License either version 1.0
