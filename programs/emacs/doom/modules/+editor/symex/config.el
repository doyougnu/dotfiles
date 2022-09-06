;;; +editor/symex/config.el -*- lexical-binding: t; -*-

(use-package! symex
  :defer t
  :config
  (symex-initialize)
  (setq browse-url-browser-function 'w3m-browse-url)
  (map! :map evil-motion-state-map
        :desc "Symex start!" "\\" #'symex-mode-interface)

  (add-hook! 'evil-normal-state-entry-hook #'symex-mode-interface)

  :custom
  (symex-common-lisp-backend 'sly)
  (symex-modal-backend 'evil))
