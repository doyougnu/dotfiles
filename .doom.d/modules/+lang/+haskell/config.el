;;; +lang/+haskell/config.el -*- lexical-binding: t; -*-

(after! haskell
  (setq haskell-process-prompt-restart   nil
        haskell-process-log              t
        haskell-interactive-popup-errors nil)

  (when (featurep! :lang haskell +lsp)
      (setq lsp-file-watch-threshold 10000
            lsp-haskell-importlens-on nil)
      (after! lsp-ui
        (setq lsp-ui-doc-position      'top)))

  (map! :localleader
        :map haskell-mode-map
        :nv "," #'haskell-process-load-file

        (:prefix ("s" . "Repl")
         "s" #'haskell-interactive-bring
         "r" #'haskell-process-restart
         "t" #'haskell-session-change-target))


  (map! :after haskell-interactive-mode
        :map haskell-interactive-mode-map
        :nv "k"   #'haskell-interactive-mode-prompt-previous
        :nv "j"   #'haskell-interactive-mode-prompt-next
        :nv "RET" #'haskell-interactive-mode-return
        :i  "C-j" #'haskell-interactive-mode-history-next
        :i  "C-k" #'haskell-interactive-mode-history-previous
        :localleader
        :nv "," #'haskell-interactive-switch-back
        :nv "c" #'haskell-interactive-mode-clear))

(set-repl-handler! 'haskell-mode #'+haskell/haskell-interactive-bring)
