;;; +editor/symex/config.el -*- lexical-binding: t; -*-

(use-package! symex
  :config
  (symex-initialize)
  (map! :map evil-motion-state-map
        :desc "Symex-interface" "\\" #'symex-mode-interface)

  :custom
  (symex-common-lisp-backend 'sly)
  (symex-modal-backend 'evil))
