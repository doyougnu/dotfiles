;;; dyg/+dyg/config.el -*- lexical-binding: t; -*-

;;; +dyg/config.el --- custom module for doom. This holds a bunch of my global
;;; settings

;;
;; Author: Jeffrey Young (doyougnu) <youngjef@oregonstate.edu>
;; URL: https://github.com/doyougnu/dyg-org
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; convieniences, we have to unbind C-k in lispy for our global key
(map! (:map (lispy-mode-map global-mode-map) "C-k" nil)
      (:map evil-normal-state-map
       (:prefix-map ("C-k" . "easy-insert")
        "i" #'dyg/char-insert
        "a" #'dyg/char-insert-after
        "w" #'dyg/word-insert
        "j" #'dyg/newline-below
        "k" #'dyg/newline-above)))

(setq doom-scratch-initial-major-mode 'org-mode)

;; set projectile to recently used and enable caching
(setq projectile-enable-caching t
      projectile-sort-order      'recently-active
      projectile-indexing-method 'hybrid)

;; always prefer newer .el files
(setq load-prefer-newer t)

;; use the text browser
(setq +lookup-open-url-fn #'eww)

;; faster which-keys
(setq which-key-idle-delay 0.15)

;; better docs from dash
(setq dash-docs-common-docsets '("Haskell" "Emacs Lisp" "Common Lisp"))

;; setup EasyPG for gpg
(setq epg-pinentry-mode 'loopback)

;; deft setup
(setq deft-directory "/home/doyougnu/sync/deft")
(setq deft-use-filename-as-title t)
(setq deft-recursive t)

(with-eval-after-load 'biblatex
  (setq org-ref-default-bibliography
        (directory-files-recursively projectile-project-root "\\.bib$")))

;; keybindings
(map! :leader
      (:prefix-map ("j" . "doyougnu")
       (:prefix ("d" . "deft")
        :desc "Deft"      "d" #'deft
        :desc "find-file" "f" #'deft-find-file
        :desc "new-file"  "n" #'deft-new-file-named)

       (:prefix ("e" . "emacs-tools")
        :desc "Byte-compile-and-load"    "c" #'emacs-lisp-byte-compile-and-load
        :desc "Byte recompile directory" "C" #'byte-recompile-directory
        :desc "Set variable"             "s" #'set-variable
        :desc "Revert buffer"            "r" #'revert-buffer
        :desc "Highlight at point"       "h" #'highlight-symbol-at-point
        :desc "Remove Highlight"         "H" #'unhighlight-regexp)

       (:prefix ("i" . "IRC")
        :desc "Init" "i" #'circe)

       "j" #'org-agenda)

      (:prefix-map ("r" . "kill-ring")
       :desc "Kill-ring" "y" #'consult-yank-pop)

      (:prefix-map ("S" . "spelling")
       :desc "Correct-word-at-point" "s" #'ispell-word
       :desc "Correct buffer"        "b" #'ispell-buffer)

      (:prefix-map ("s" . "search")
       :desc "Search project for point" "p" #'+default/search-project-for-symbol-at-point)

      (:when (featurep! :tools lsp)
       :desc "lsp-imenu" "cI" #'lsp-ui-imenu)

      :desc "Org-capture" "SPC" #'org-capture
      :desc "Ranger"      "."   #'ranger)

(map! (:map company-active-map
      "C-l" #'company-complete-selection
      "C-;" #'fill-paragraph)
      (:map minibuffer-local-map
       "C-l" #'vertico-directory-enter
       "C-h" #'vertico-directory-up)
      (:map evil-markdown-mode-map
       :nv "C-;" #'fill-paragraph))

(map! :map global-map
       :nv "C-;" #'fill-paragraph)

(map!
 :after wordnut
 :map wordnut-mode-map
 :nv "q" #'quit-window
 :localleader
 :nv "p" #'wordnut-history-backword
 :nv "n" #'wordnut-history-forward
 :nv "h" #'describe-mode
 :nv "o" #'wordnut-show-overview
 :nv "l" #'wordnut-history-lookup)

(map!
 :after with-editor
 :map with-editor-mode-map
 :localleader
 "," #'with-editor-finish
 "k" #'with-editor-cancel)

;; use projectile for SPC p c
(define-key!
  [remap projectile-compile-project] #'projectile-compile-project)

;; tinkering with vertico + consult
(advice-add #'completing-read-multiple
            :override #'consult-completing-read-multiple)

;; force ivy to not search until 3 chars have been input
(after! ivy
  (setq ivy-more-chars-alist '((counsel-grep   . 3)
                               (counsel-rg     . 3)
                               (counsel-search . 3)
                               (t              . 3))))
