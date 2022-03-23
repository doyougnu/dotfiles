;;; +editor/multi-compile/config.el -*- lexical-binding: t; -*-

(use-package! multi-compile
  :config
  (map! :leader
        :map doom-leader-code-map
        "c c" #'multi-compile-run))
