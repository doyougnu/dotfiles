;;; +lang/haskell/config.el -*- lexical-binding: t; -*-

(after! haskell
  (setq haskell-process-prompt-restart nil)
  (when (featurep! :tools lsp)
    (setq lsp-file-watch-threshold 10000)))
