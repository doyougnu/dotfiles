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
(map! (:after lispy
       (:map lispy-mode-map "C-k" nil)
       (:map evil-normal-state-map
        (:prefix-map ("C-k" . "easy-insert")
        "i" #'dyg/char-insert
        "a" #'dyg/char-insert-after
        "w" #'dyg/word-insert
        "j" #'dyg/newline-below
        "k" #'dyg/newline-above))))

(setq doom-scratch-initial-major-mode 'org-mode)

;; always prefer newer .el files
(setq load-prefer-newer t)

;; faster which-keys
(setq which-key-idle-delay 0.15)

;; nice whitespace
(setq whitespace-style '(face tabs spaces trailing
      space-before-tab newline indentation empty space-after-tab
      space-mark tab-mark))

(add-hook 'tuareg-mode-hook   'dyg/set-up-whitespace-handling)
(add-hook 'c-mode-hook        'dyg/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode    'dyg/set-up-whitespace-handling)
(add-hook 'java-mode-hook     'dyg/set-up-whitespace-handling)
(add-hook 'lua-mode-hook      'dyg/set-up-whitespace-handling)
(add-hook 'rust-mode-hook     'dyg/set-up-whitespace-handling)
(add-hook 'scala-mode-hook    'dyg/set-up-whitespace-handling)
;; increases input latency too much for some reason
;; (add-hook 'markdown-mode-hook 'dyg/set-up-whitespace-handling)
(add-hook 'org-mode-hook      'dyg/set-up-whitespace-handling)
(add-hook 'haskell-mode-hook  'dyg/set-up-whitespace-handling)
(add-hook 'python-mode-hook   'dyg/set-up-whitespace-handling)
(add-hook 'scheme-mode-hook   'dyg/set-up-whitespace-handling)
(add-hook 'erlang-mode-hook   'dyg/set-up-whitespace-handling)
(add-hook 'asm-mode-hook      'dyg/set-up-whitespace-handling)
(add-hook 'nasm-mode-hook     'dyg/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook     'dyg/set-up-whitespace-handling)

;; setup EasyPG for gpg
(setq epg-pinentry-mode 'loopback)

;; deft setup
(setq deft-directory "/home/doyougnu/sync/.deft")
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
        :desc "Remove Highlight"         "H" #'unhighlight-regexp
        )

       (:prefix ("i" . "IRC")
        :desc "Init" "i" #'=irc)

       "j" #'org-agenda)

      (:prefix-map ("r" . "kill-ring")
       :desc "Kill-ring" "y" #'counsel-yank-pop)

      (:prefix-map ("S" . "spelling")
       :desc "Correct-word-at-point" "s" #'ispell-word
       :desc "Correct buffer"        "b" #'ispell-buffer)

      (:prefix-map ("s" . "search")
       :desc "Search project for point" "p" #'+default/search-project-for-symbol-at-point)

      (:when (featurep! :tools lsp)
       :desc "lsp-imenu" "cI" #'lsp-ui-imenu)

      :desc "Org-capture" "SPC" #'org-capture)

(map! (:map company-active-map
      "C-l" #'company-complete-selection
      "C-;" #'fill-paragraph)
      (:map minibuffer-local-map
       "C-l" #'minibuffer-complete-and-exit)
      (:map vertico-map
       "C-l" #'vertico-insert)
      (:map evil-markdown-mode-map
       :nv "C-;" #'fill-paragraph)
      (:map global-map
       :nv "C-;" #'fill-paragraph))

(map!
 :after with-editor
 :map with-editor-mode-map
 :localleader
 "," #'with-editor-finish
 "k" #'with-editor-cancel)

;; tinkering with vertico + consult
(advice-add #'completing-read-multiple
            :override #'consult-completing-read-multiple)

;; force ivy to not search until 3 chars have been input
(after! ivy
  (setq ivy-more-chars-alist '((counsel-grep   . 3)
                               (counsel-rg     . 3)
                               (counsel-search . 3)
                               (t              . 3))))
