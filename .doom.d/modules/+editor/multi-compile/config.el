;;; +editor/multi-compile/config.el -*- lexical-binding: t; -*-

(use-package! multi-compile
  :config
  (map! :leader
        :map doom-leader-code-map
        "c c" #'multi-compile-run)
  (setq multi-compile-alist
        (append multi-compile-alist
                '(
                  ;; GHC target
                  ((string/starts-with buffer-file-name "/home/doyougnu/programming/ghc")
                   ;; matched actions
                   . (("Hadrian/Ghci"   "nix-shell --pure --run './hadrian/ghci'"
                        (projectile-root-bottom-up buffer-file-name))
                      ("Clean and Boot" "nix-shell --pure --run 'hadrian/build clean && ./configure && ./boot'"
                       (projectile-root-bottom-up buffer-file-name))
                      ("Perf Build"     "nix-shell --pure --run 'hadrian/build -j12 --flavour=perf'"
                       (projectile-root-bottom-up buffer-file-name))
                      ))
                  ))))
