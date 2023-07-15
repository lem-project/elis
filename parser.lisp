(defpackage :elis/parser
  (:use :cl)
  (:export :parse))
(in-package :elis/parser)

(defvar *eof-value* '#:eof)

(defvar *elis-readtable*
  (let ((readtable (copy-readtable)))
    (setf (readtable-case readtable) :preserve)
    (set-macro-character #\. 'dot-reader nil readtable)
    (set-macro-character #\| 'char-reader nil readtable)
    readtable))

(defun char-reader (stream char)
  (declare (ignore stream))
  (case char
    (#\| :pipe)
    (otherwise (string char))))

(defun dot-reader (stream char)
  (declare (ignore char))
  (cond ((eql #\. (peek-char nil stream nil))
         (read-char stream)
         :parent-directory)
        (t
         :current-directory)))

(defun read-ahead (stream)
  (let ((*read-eval* nil)
        (*readtable* *elis-readtable*))
    (read stream nil *eof-value*)))

(defun tokenize (string)
  (with-input-from-string (stream string)
    (loop :for arg := (read-ahead stream)
          :until (eq arg *eof-value*)
          :collect (typecase arg
                     (keyword arg)
                     (symbol (string arg))
                     (otherwise arg)))))

(defun construct-execute (command)
  (cons :execute command))

(defun parse (string)
  (let* ((tokens (tokenize string))
         (commands (split-sequence:split-sequence :pipe tokens)))
    (if (alexandria:length= commands 1)
        (construct-execute (first commands))
        (cons :pipe
              (mapcar #'construct-execute
                      commands)))))
