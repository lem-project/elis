(defpackage :lem-tests/elis/parser
  (:use :cl :rove)
  (:local-nicknames (:parser :elis/parser)))
(in-package :lem-tests/elis/parser)

(deftest parse-test
  (ok (equal '(:execute "ls")
             (parser:parse "ls")))

  (ok (equal '(:execute "ls" "-a")
             (parser:parse "ls -a")))

  (ok (equal '(:execute "ls" "foo")
             (parser:parse "ls \"foo\"")))

  (ok (equal '(:execute "ls" "foo bar")
             (parser:parse "ls  \"foo bar\"")))

  (ok (equal '(:execute "ls" "foo \"bar")
             (parser:parse "ls  \"foo \\\"bar\"")))

  (ok (equal '(:execute "ls" :current-directory)
             (parser:parse "ls .")))

  (ok (equal '(:execute "cd" :parent-directory)
             (parser:parse "cd ..")))

  (ok (equal '(:execute "foo" :parent-directory :current-directory)
             (parser:parse "foo .. .")))

  (ok (equal '(:execute "foo" :parent-directory "bar")
             (parser:parse "foo .. bar")))

  (ok (equal '(:execute "foo" :current-directory "bar")
             (parser:parse "foo . bar")))

  (ok (equal '(:pipe
               (:execute "find" :current-directory)
               (:execute "wc" "-l"))
             (parser:parse "find . | wc -l"))))

(deftest parse-error-test
  (ok (signals (parser:parse "")
               'elis/conditions:parse-input-error))
  (ok (signals (parser:parse "  ")
               'elis/conditions:parse-input-error)))
