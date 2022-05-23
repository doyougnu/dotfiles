;;; dyg/+flycheck/config.el -*- lexical-binding: t; -*-

(map! :after flycheck
      :leader
      (:prefix-map ("e" . "errors")
       :desc "list errors"      "l" #'flycheck-list-errors
       :desc "next error"       "n" #'flycheck-next-error
       :desc "previous error"   "p" #'flycheck-previous-error
       :desc "select checker"   "f" #'flycheck-select-checker
       :desc "check buffer"     "b" #'flycheck-buffer
       :desc "clear flycheck"   "C" #'flycheck-clear
       :desc "describe checker" "d" #'flycheck-describe-checker))
