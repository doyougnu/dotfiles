;;; +editor/symex/config.el -*- lexical-binding: t; -*-

(use-package! symex
  :config
  (symex-initialize)
  (setq browse-url-browser-function 'w3m-browse-url)
  (map! :map evil-motion-state-map
        :desc "Symex start!" "\\" #'symex-mode-interface)

  :custom
  (symex-common-lisp-backend 'sly)
  (symex-modal-backend 'evil))
