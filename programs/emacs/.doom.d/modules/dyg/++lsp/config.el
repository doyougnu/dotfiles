;;; dyg/++lsp/config.el -*- lexical-binding: t; -*-

(after! (:and haskell lsp)
  (setq lsp-file-watch-threshold 10000)
  (setq lsp-ui-doc-position      'top))
