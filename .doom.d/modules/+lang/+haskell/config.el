;;; +lang/+haskell/config.el -*- lexical-binding: t; -*-

(after! haskell
  (setq haskell-process-prompt-restart nil
        haskell-process-log              t)

  (when (featurep! :lang haskell +lsp)
      (setq lsp-file-watch-threshold 10000)
      (after! lsp-ui
        (setq lsp-ui-doc-position      'top))))
