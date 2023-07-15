(defpackage :elis/executor
  (:use :cl)
  (:export :execute))
(in-package :elis/executor)

(defvar *process-list* '())

(defvar *write-callback*)
(defvar *exit-process-callback*)

(defgeneric execute-aux (operator arguments))

(defun run-process (command)
  (let ((process (async-process:create-process command :nonblock nil)))
    (push process *process-list*)
    (bt:make-thread (lambda ()
                      (loop
                        (unless (async-process:process-alive-p process)
                          (alexandria:deletef *process-list* process)
                          (funcall *exit-process-callback*)
                          (return))
                        (alexandria:when-let
                            (string (async-process:process-receive-output process))
                          (funcall *write-callback* string))))
                    :name (format nil "elis run-process ~S" command)
                    :initial-bindings `((*write-callback* . ,*write-callback*)
                                        (*exit-process-callback* . ,*exit-process-callback*)
                                        ,@bt:*default-special-bindings*))))

(defmethod execute-aux ((operator (eql :execute)) arguments)
  (destructuring-bind (command-name &rest args) arguments
    (let ((process
            (run-process
             (cons command-name
                   (mapcar (lambda (arg)
                             (case arg
                               (:current-directory (namestring (probe-file ".")))
                               (:parent-directory (namestring (probe-file "..")))
                               (otherwise (maybe-expand-path arg))))
                           args)))))
      process)))

(defun execute (string &key ((:write-callback *write-callback*)
                             (error "Missing :write-callback"))
                            ((:exit-process-callback *exit-process-callback*)
                             (error "Missing :exit-process-callback")))
  (let ((expression (elis/parser:parse string)))
    (execute-aux (first expression)
                 (rest expression))))

(defun homedir (&optional (trim-slash t))
  (if trim-slash
      (string-right-trim '(#\/) (namestring (user-homedir-pathname)))
      (namestring (user-homedir-pathname))))

(defun maybe-expand-path (string)
  (cond ((alexandria:starts-with-subseq "~/" string)
         (concatenate 'string
                      (homedir nil)
                      (subseq string 2)))
        ((equal string "~")
         (homedir t))
        (t
         string)))
