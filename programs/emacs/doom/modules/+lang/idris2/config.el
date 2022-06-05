;;; +lang/idris2/config.el -*- lexical-binding: t; -*-

;;; Code:
(use-package! idris2-mode
  :defer t
  :config

  (when (featurep! +lsp)
    (add-hook 'idris2-mode-hook #'lsp!))
  (advice-add #'evil-motion-range :around #'~/evil-motion-range--wrapper)

  (set-repl-handler! 'idris2-mode 'idris2-pop-to-repl)

  (set-lookup-handlers! 'idris2-mode
    :documentation #'idris2-docs-at-point)

  (map! (:map idris2-mode-map
         :localleader
         :nv "," #'idris2-load-file
         :nv ">" #'prog-indent-sexp
         :nv "s" #'idris2-proof-search
         :nv "a" #'idris2-add-clause
         :nv "l" #'idris2-make-lemma
         :nv "'" #'idris2-pop-to-repl
         :nv "w" #'idris2-make-with-block
         :nv "n" #'idris2-load-forward-line
         :nv "p" #'idris2-load-backward-line
         :nv "c" #'idris2-case-dwim
         :nv "SPC" #'idris2-newline-and-indent
         (:prefix ("e" . "errors")
          :nv "n" #'idris2-next-error
          :nv "p" #'idris2-previous-error)
         (:prefix ("b" .  "build")
          :nv "b" #'idris2-ipkg-build
          :nv "c" #'idris2-ipkg-clean
          :nv "p" #'idris2-open-package-file
          :nv "i" #'idris2-ipkgs-install)
         (:prefix ("d" . "docs")
          :nv "d"   #'idris2-docs-at-point
          :nv "t"   #'idris2-type-search
          :nv "a"   #'idris2-apropos))
        (:map idris2-ipkg-mode-map
         :localleader
         :nv "b" #'idris2-ipkg-build
         :nv "c" #'idris2-ipkg-clean
         :nv "p" #'idris2-ipkg-open
         :nv "i" #'idris2-ipkg-install
         :nv "," #'idris2-ipkg-insert-field)
        (:map (idris2-tree-info-mode-map
               idris2-info-mode-map
               idris2-hole-list-mode-map
               idris2-repl-mode-map
               idris2-compiler-notes-mode-map)
         :localleader
         :nv "'"   #'idris2-pop-to-repl
         :nv "t"   #'idris2-type-at-point
         :nv "SPC" #'prop-menu-by-completing-read
         :nv "i"   #'idris2-show-term-implicits
         :nv "I"   #'idris2-hide-term-implicits
         :nv "n"   #'idris2-normalize-term
         :nv "c"   #'idris2-show-core-term
         (:prefix ("d" . "docs")
          :nv "d"   #'idris2-docs-at-point
          :nv "t"   #'idris2-type-search
          :nv "a"   #'idris2-apropos))
        (:map idris2-hole-list-mode-map
         :nvi "q"  #'idris2-hole-list-quit
         :localleader
         :nv ","  #'idris2-compiler-notes-default-action-or-show-details)
        (:map idris2-info-mode-map
         :nvi "q"  #'idris2-info-quit
         :localleader
         :nv ","  #'prop-menu-show-menu)
        (:map idris2-repl-mode-map
         :i  "C-n" #'idris2-repl-forward-history
         :i  "C-p" #'idris2-repl-backward-history
         :i  "C-l" #'completion-at-point
         :nv "C-n" #'idris2-repl-forward-history
         :nv "C-p" #'idris2-repl-backward-history
         :nv "0"   #'idris2-repl-begin-of-prompt
         :nv "C-l" #'completion-at-point
         :localleader
         :nv ","   #'idris2-repl-clear-buffer)
        (:map idris2-prover-script-mode-map
         :nv "k"    #'idris2-prover-abandon
         :nv "q"    #'idris2-prover-script-qed
         :nvi "C-j" #'idris2-prover-script-forward
         :nvi "C-k" #'idris2-prover-script-backward
         :nvi "C-n" #'completion-at-point)
        (:map idris2-compiler-notes-mode-map
         :i "q"   #'idris2-notes-quit
         :nv "q"  #'idris2-notes-quit)))
