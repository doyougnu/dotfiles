;;; +lang/elisp/config.el -*- lexical-binding: t; -*-

(map!
 :after elisp-mode
 :localleader
 :map emacs-lisp-mode-map
 :desc "ielm" "'" #'ielm/begin
 :nv "," #'ielm/eval-current-form

 (:prefix ("e" . "Eval")
  "r" #'ielm/send-region
  "e" #'ielm/eval-current-form
  "b" #'eval-buffer
  "d" #'eval-defun
  "c" #'ielm/clear)

 (:prefix ("h" . "Help")
  "h" #'helpful-at-point)

 )
