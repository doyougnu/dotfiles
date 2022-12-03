;; -*- no-byte-compile: t; -*-
;;; +editor/org-mru-clock/packages.el

(use-package! org-mru-clock
  :config
  (require 'embark)
  (map! :leader
        (:prefix-map ("n" . "notes")
                     (:prefix-map ("c" . "clock")
                      :desc "clock-in" "i" #'org-mru-clock-in
                      :desc "clock-out" "o" #'org-clock-out
                      :desc "goto-clock" "g" #'org-clock-goto
                      :desc "recent-clock" "r" #'org-mru-clock-select-recent-task)))

  (setq org-mru-clock-how-many 15
        org-mru-clock-keep-formatting t
        org-mru-clock-predicate #'org-entry-is-todo-p))
