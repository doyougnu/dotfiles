;;; +lang/elisp/config.el -*- lexical-binding: t; -*-

(map!
 :after elisp-mode
 :localleader
 :map emacs-lisp-mode-map
 :desc "ielm" "'" #'ielm/begin
 :nv "," #'ielm/defun

 (:prefix ("e" . "Eval")
  "r" #'ielm/send-region
  "e" #'ielm/eval-current-form
  "b" #'eval-buffer
  "d" #'eval-defun
  "c" #'ielm/clear)

 (:prefix ("d" . "debug")
          "i" #'+emacs-lisp/edebug-instrument-defun-on
          "I" #'+emacs-lisp/edebug-instrument-defun-off
          "A" #'edebug-all-defuns
          "a" #'edebug-all-forms
          "m" #'edebug-set-initial-mode
          "b" #'edebug-set-breakpoint
          "B" #'edebug-set-conditional-breakpoint)

 (:prefix ("h" . "Help")
  "h" #'helpful-at-point)

 )
