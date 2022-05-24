;;; +lang/idris2/config.el -*- lexical-binding: t; -*-

(use-package! idris2-mode
  :defer t
  :config

  ;; fix evil delay
  (advice-add #'evil-motion-range :around #'~/evil-motion-range--wrapper)

  ;; keybinds
  (map! :map idris2-mode-map
        :localleader
        "n" #'idris2-next-error
        "p" #'idris2-previous-error
        "l" #'idris2-load-file
        "w" #'idris2-make-with-block
        "," #'idris2-repl
        "'" #'idris2-repl

        (:prefix ("p" . "Proofs")
         "a" #'idris2-proof-search
         "c" #'idris2-case-dwim
         "l" #'idris2-make-lemma
         "a" #'idris2-add-clause)

        (:prefix ("b" . "Build")
         "b" #'idris2-ipkg-build
         "c" #'idris2-ipkg-clean
         "i" #'idris2-ipkgs-install
         "p" #'idris2-open-package-file)

        (:prefix ("h" . "Help")
         "h" #'idris2-docs-at-point
         "s" #'idris2-type-search
         "t" #'idris2-type-at-point)))
