;;; +editor/symex/config.el -*- lexical-binding: t; -*-

(use-package! symex
  :config
  (symex-initialize)
  (map! :leader
        :desc "Symex-interface" "\\" #'symex-mode-interface))
