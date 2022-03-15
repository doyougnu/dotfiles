;;; +editor/symex/config.el -*- lexical-binding: t; -*-

(use-package! symex
  :config
  (symex-initialize)
  (setq symex-common-lisp-backend 'sly)
  (map! :leader
        :desc "Symex-interface" "\\" #'symex-mode-interface)

  :custom
  (symex-modal-backend 'evil))
