;;; +lang/idris2/config.el -*- lexical-binding: t; -*-

(use-package! idris2-mode
  :config
  (when (featurep! +lsp)
    (add-hook 'idris2-mode-hook #'lsp!))
  (advice-add #'evil-motion-range :around #'~/evil-motion-range--wrapper)

  (set-repl-handler! 'idris2-mode 'idris2-pop-to-repl)
  (set-lookup-handlers! 'idris2-mode
    :documentation #'idris2-docs-at-point))
;; (after! idris2-mode

;;   (setq idris2-interpreter-path "idris2")


;;   (map! (:map idris2-mode-map
;;                 :localleader
;;                 :nv "," #'idris2-load-file
;;                 :nv "a" #'idris2-add-clause
;;                 :nv "'" #'idris2-pop-to-repl
;;                 (:prefix ("b" . "build")
;;                   :nv "b" #'idris2-ipkg-build
;;                   :nv "c" #'idris2-ipkg-clean
;;                   :nv "i" #'idris2-ipkg-install
;;                   :nv "," #'idris2-ipkg-insert-field)
;;                 (:prefix ("d" . "docs")
;;                   :nv "t"   #'idris2-docs-at-point
;;                   :nv "s"   #'idris2-type-search
;;                   :nv "a"   #'idris2-apropos)
;;                 (:prefix ("m" . "Active terms")
;;                   :nv "i"   #'idris2-show-term-implicits
;;                   :nv "I"   #'idris2-hide-term-implicits
;;                   :nv "n"   #'idris2-normalize-term
;;                   :nv "c"   #'idris2-show-core-term))
;;         (:map idris2-ipkg-mode-map
;;                 :localleader
;;                 :nv "b" #'idris2-ipkg-build
;;                 :nv "c" #'idris2-ipkg-clean
;;                 :nv "i" #'idris2-ipkg-install
;;                 :nv "," #'idris2-ipkg-insert-field)
;;         (:map (idris2-tree-info-mode-map
;;                idris2-info-mode-map
;;                idris2-hole-list-mode-map
;;                idris2-repl-mode-map
;;                idris2-compiler-notes-mode-map)
;;                 :localleader
;;                 :nv ","   #'idris2-pop-to-repl
;;                 :nv "t"   #'idris2-type-at-point
;;                 :nv "h"   #'idris2-docs-at-point
;;                 :nv "s"   #'idris2-type-search
;;                 :nv "a"   #'idris2-apropos
;;                 :nv "SPC" #'prop-menu-by-completing-read
;;                 :nv "i"   #'idris2-show-term-implicits
;;                 :nv "I"   #'idris2-hide-term-implicits
;;                 :nv "n"   #'idris2-normalize-term
;;                 :nv "c"   #'idris2-show-core-term)
;;         (:map idris2-tree-info-mode-map
;;                 :nv "q"  #'idris2-tree-info-quit)
;;         (:map idris2-info-mode-map
;;                 :nv "q"  #'idris2-info-quit)
;;         (:map idris2-repl-mode-map
;;                 :i  "C-n" #'idris2-repl-forward-history
;;                 :i  "C-p" #'idris2-repl-backward-history
;;                 :i  "C-l" #'completion-at-point
;;                 :nv "C-n" #'idris2-repl-forward-history
;;                 :nv "C-p" #'idris2-repl-backward-history
;;                 :nv "C-l" #'completion-at-point
;;                 :localleader
;;                 :nv ","   #'idris2-repl-clear-buffer)
;;         (:map idris2-prover-script-mode-map
;;                 :nv "k"    #'idris2-prover-abandon
;;                 :nv "q"    #'idris2-prover-script-qed
;;                 :nvi "C-j" #'idris2-prover-script-forward
;;                 :nvi "C-k" #'idris2-prover-script-backward
;;                 :nvi "C-n" #'completion-at-point)
;;         (:map idris2-compiler-notes-mode-map
;;                 :i "q"   #'idris2-notes-quit
;;                 :nv "q"  #'idris2-notes-quit))
;;   )
