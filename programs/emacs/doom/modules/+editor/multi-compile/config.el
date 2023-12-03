;;; +editor/multi-compile/config.el -*- lexical-binding: t; -*-

(use-package! multi-compile
  :config
  (map! :leader
        :map doom-leader-code-map
        "c c" #'multi-compile-run)
  (setq multi-compile-template
        (append multi-compile-template
                '(("%test" . "Which test to test?: "))))
  (setq multi-compile-alist
        (append multi-compile-alist
                '(
                  ;; GHC target
                  ((string/starts-with "ghc" buffer-file-name)
                   ;; matched actions
                   . (("Hadrian/Ghci"   "./hadrian/ghci"
                        (projectile-root-bottom-up buffer-file-name))
                      ("Clean and Boot" "hadrian/build clean && ./configure && ./boot"
                       (projectile-root-bottom-up buffer-file-name))
                      ("Perf Build"     "hadrian/build -j12 --flavour=perf"
                       (projectile-root-bottom-up buffer-file-name))
                      ("JS Clean"     "hadrian/build clean --build-root=_js-backend"
                       (projectile-root-bottom-up buffer-file-name))
                      ("JS Configure"     "emconfigure ./configure --target=js-unknown-ghcjs"
                       (projectile-root-bottom-up buffer-file-name))
                      ("JS Build"     "hadrian/build -j12 --flavour=quick-js+omit_pragmas --build-root=_js-backend --bignum=native --docs=none \"stage1.*.ghc.hs.opts += -ddump-stg-cg -ddump-js -ddump-to-file\""
                       (projectile-root-bottom-up buffer-file-name))))

                  (forth-mode . (("forth compile"      . "gforth %file-name")))

                  (latex-mode . (("latex build buffer" . "latexmk -pdf -shell-escape %file-name")
                                 ("latex build paper"  . "latexmk -pdf -shell-escape paper.tex")
                                 ("latex clean"        . "latexmk -c")
                                 ("latex nix-build" . "nix-build")))
                  ))))

;;; config.el ends here
