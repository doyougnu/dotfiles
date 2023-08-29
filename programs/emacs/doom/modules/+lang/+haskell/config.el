;;; +lang/+haskell/config.el -*- lexical-binding: t; -*-

(after! projectile
(setq haskell-process-suggest-restart  nil
      haskell-process-log              t
      haskell-interactive-popup-errors nil)

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

      (projectile-register-project-type 'ghc '("ghc.mk" "compiler" "nofib" "rules" "rts" "utils" "testsuite")
                                        :project-file "compiler/ghc.cabal.in"
                                        :compile      "hadrian/build -j12 --flavour=perf"
                                        :src-dir      "compiler"
                                        :configure    "hadrian/build clean && ./boot && ./configure"
                                        :run          "hadrian/ghci")

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
        :nv "t"   #'haskell-interactive-mode-prompt-previous
        :nv "h"   #'haskell-interactive-mode-prompt-next
        :nv "RET" #'haskell-interactive-mode-return
        :i  "C-t" #'haskell-interactive-mode-history-next
        :i  "C-h" #'haskell-interactive-mode-history-previous
        :localleader
        :nv "," #'haskell-interactive-switch-back
        :nv "c" #'haskell-interactive-mode-clear)

(set-repl-handler! 'haskell-mode #'+haskell/haskell-interactive-bring)
(add-hook! 'haskell-mode-hook #'haskell-unicode-input-method-enable)
(setq haskell-font-lock-symbols t)
)
