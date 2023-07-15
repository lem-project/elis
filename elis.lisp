(defpackage :lem/elis
  (:use :cl
        :lem))
(in-package :lem/elis)

(define-major-mode elis-mode ()
    (:name "ELIS"
     :keymap *elis-mode-keymap*
     ;; TODO: syntax-table
     )
  (lem/listener-mode:start-listener-mode (merge-pathnames "history/elis" (lem-home)))
  (setf (variable-value 'lem/listener-mode:listener-set-prompt-function
                        :buffer (current-buffer))
        'set-prompt
        (variable-value 'lem/listener-mode:listener-check-input-function
                        :buffer (current-buffer))
        'input-complete-p
        (variable-value 'lem/listener-mode:listener-execute-function
                        :buffer (current-buffer))
        'elis-execute))

(defun set-prompt (point)
  (insert-string point "$ "))

(defun input-complete-p (point)
  (declare (ignore point))
  t)

(defun elis-execute (point string)
  (message "~A" string)
  (lem/listener-mode:refresh-prompt (point-buffer point) t))

(define-command elis () ()
  (lem/listener-mode:listener-start "*elis*" 'elis-mode))
