;;; +lang/+idris/config.el -*- lexical-binding: t; -*-

(after! idris-mode

  (setq idris-interpreter-path "idris2")

  (map! (:map idris-mode-map
                :localleader
                :nv "," #'idris-load-file
                :nv "'" #'idris-pop-to-repl)
        (:map idris-ipkg-mode-map
                :localleader
                :nv "b" #'idris-ipkg-build
                :nv "c" #'idris-ipkg-clean
                :nv "i" #'idris-ipkg-install
                :nv "," #'idris-ipkg-insert-field)
        (:map (idris-tree-info-mode-map
               idris-info-mode-map
               idris-hole-list-mode-map
               idris-repl-mode-map
               idris-compiler-notes-mode-map)
                :localleader
                :nv ","   #'idris-pop-to-repl
                :nv "t"   #'idris-type-at-point
                :nv "h"   #'idris-docs-at-point
                :nv "s"   #'idris-type-search
                :nv "a"   #'idris-apropos
                :nv "SPC" #'prop-menu-by-completing-read
                :nv "i"   #'idris-show-term-implicits
                :nv "I"   #'idris-hide-term-implicits
                :nv "n"   #'idris-normalize-term
                :nv "c"   #'idris-show-core-term)
        (:map idris-tree-info-mode-map
                :nv "q"  #'idris-tree-info-quit)
        (:map idris-info-mode-map
                :nv "q"  #'idris-info-quit)
        (:map idris-repl-mode-map
                :i  "C-n" #'idris-repl-forward-history
                :i  "C-p" #'idris-repl-backward-history
                :i  "C-l" #'completion-at-point
                :nv "C-n" #'idris-repl-forward-history
                :nv "C-p" #'idris-repl-backward-history
                :nv "C-l" #'completion-at-point
                :localleader
                :nv ","   #'idris-repl-clear-buffer)
        (:map idris-prover-script-mode-map
                :nv "k"    #'idris-prover-abandon
                :nv "q"    #'idris-prover-script-qed
                :nvi "C-j" #'idris-prover-script-forward
                :nvi "C-k" #'idris-prover-script-backward
                :nvi "C-n" #'completion-at-point)
        (:map idris-compiler-notes-mode-map
                :i "q"   #'idris-notes-quit
                :nv "q"  #'idris-notes-quit))
  )
