# clj-git

FIXME: description

## Examples

### Modify a file and commit the change

```clojure
(print-status)
Nothing to report, working directory clean

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

## License

Copyright © 2016

Distributed under the Eclipse Public License either version 1.0
