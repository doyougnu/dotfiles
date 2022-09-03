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
                   . (("Hadrian/Ghci"   "nix-shell --pure --run './hadrian/ghci'"
                        (projectile-root-bottom-up buffer-file-name))
                      ("Clean and Boot" "nix-shell --pure --run 'hadrian/build clean && ./configure && ./boot'"
                       (projectile-root-bottom-up buffer-file-name))
                      ("Perf Build"     "nix-shell --pure --run 'hadrian/build -j12 --flavour=perf'"
                       (projectile-root-bottom-up buffer-file-name))
                      ("JS Configure"     "nix-shell --pure --run 'emconfigure ./configure --target=js-unknown-ghcjs'"
                       (projectile-root-bottom-up buffer-file-name))
                      ("JS Build"     "nix-shell --run 'hadrian/build -j12 --flavour=quick-js+omit_pragmas --build-root=_js-backend --bignum=native --docs=none \"stage1.*.ghc.hs.opts += -ddump-stg-cg -ddump-js -ddump-to-file\"'"
                       (projectile-root-bottom-up buffer-file-name))
                      ("JS Test"     "nix-shell --run 'hadrian/build -j12 --flavour=quick-js+omit_pragmas --build-root=_js-backend --bignum=native --docs=none \"stage1.*.ghc.hs.opts += -ddump-stg-cg -ddump-js -ddump-to-file\" --only=%test'"
                       (projectile-root-bottom-up buffer-file-name))
                      ))
                  ))))

;;; config.el ends here
