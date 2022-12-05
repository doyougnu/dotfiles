;; -*- no-byte-compile: t; -*-
;;; +editor/org-mru-clock/packages.el

(use-package! org-mru-clock
  :defer t
  :config
  ;; (after! embark
  ;;   (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook))

  (setq org-mru-clock-how-many 15
        org-mru-clock-keep-formatting t
        org-mru-clock-predicate #'org-entry-is-todo-p))
