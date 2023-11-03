;;; +lang/+haskell/config.el -*- lexical-binding: t; -*-

(after! projectile

  (projectile-register-project-type 'ghc '("ghc.mk" "compiler" "nofib" "rules" "rts" "utils" "testsuite")
                                    :project-file "compiler/ghc.cabal.in"
                                    :compile      "hadrian/build -j12 --flavour=perf"
                                    :src-dir      "compiler"
                                    :configure    "hadrian/build clean && ./boot && ./configure"
                                    :run          "hadrian/ghci"))

(when (modulep! :lang haskell +lsp)
  (setq lsp-file-watch-threshold 10000
        lsp-haskell-plugin-import-lens-code-lens-on    nil
        lsp-haskell-plugin-import-lens-code-actions-on nil
        lsp-haskell-plugin-ghcide-type-lenses-global-on nil)

  (after! lsp-mode
    (setq lsp-file-watch-ignored-directories
          (append '("/store/Programming/ghc/_build"
                    "/store/Programming/ghc/_ticky"
                    "/store/Programming/ghc/_validate"
                    "/store/Programming/ghc/testsuite"
                    "/store/Programming/ghc/docs"
                    "/store/Programming/ghc/nofib"
                    "/store/Programming/ghc/")
                  lsp-file-watch-ignored-directories)))

  (after! lsp-ui
    (setq lsp-ui-doc-position      'top)))

(setq haskell-process-suggest-restart  nil
      haskell-process-log              t
      haskell-interactive-popup-errors nil
      ;; this is necessary because haskell-mode hasn't updated yet. Check if
      ;; this variable is need later
      flymake-allowed-file-name-masks '(("\\.l?hs\\'" haskell-flymake-init)
                                       ("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-proc-simple-make-init nil flymake-proc-real-file-name-considering-includes)
                                       ("\\.xml\\'" flymake-proc-xml-init)
                                       ("\\.html?\\'" flymake-proc-xml-init)
                                       ("\\.cs\\'" flymake-proc-simple-make-init)
                                       ("\\.php[345]?\\'" flymake-proc-php-init)
                                       ("\\.h\\'" flymake-proc-master-make-header-init flymake-proc-master-cleanup)
                                       ("\\.java\\'" flymake-proc-simple-make-java-init flymake-proc-simple-java-cleanup)
                                       ("[0-9]+\\.tex\\'" flymake-proc-master-tex-init flymake-proc-master-cleanup)
                                       ("\\.tex\\'" flymake-proc-simple-tex-init)
                                       ("\\.idl\\'" flymake-proc-simple-make-init))
      )

(map! :localleader
      :map haskell-mode-map
      :nv "," #'haskell-process-load-file

      (:prefix ("s" . "Repl")
               "s" #'haskell-interactive-bring
               "r" #'haskell-process-restart
               "t" #'haskell-session-change-target))


(map! :after haskell-interactive-mode
      :map haskell-interactive-mode-map
      :nv "t"   #'haskell-interactive-mode-prompt-previous
      :nv "h"   #'haskell-interactive-mode-prompt-next
      :nv "RET" #'haskell-interactive-mode-return
      :i  "C-t" #'haskell-interactive-mode-history-next
      :i  "C-h" #'haskell-interactive-mode-history-previous
      :localleader
      :nv "," #'haskell-interactive-switch-back
      :nv "c" #'haskell-interactive-mode-clear)

(set-repl-handler! 'haskell-mode #'+haskell/haskell-interactive-bring)
