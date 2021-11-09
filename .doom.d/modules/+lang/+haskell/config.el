;;; +lang/haskell/config.el -*- lexical-binding: t; -*-

(after! haskell
  (setq haskell-process-prompt-restart nil
        haskell-process-log              t)

  (when (featurep! :tools lsp)
    (setq lsp-file-watch-threshold 10000)))
