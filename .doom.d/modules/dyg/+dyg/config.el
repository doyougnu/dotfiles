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
        "C-j" #'dyg/newline-below
        "C-k" #'dyg/newline-above)))

(setq doom-scratch-initial-major-mode 'org-mode)

;; always prefer newer .el files
(setq load-prefer-newer t)

;; faster which-keys
(setq which-key-idle-delay 0.15)

;; deft setup
(setq deft-directory "/home/doyougnu/sync/org/.deft")
(setq deft-use-filename-as-title t)
(setq deft-recursive t)

(with-eval-after-load 'biblatex
  (setq org-ref-default-bibliography
        (directory-files-recursively projectile-project-root "\\.bib$")))

;; keybindings
(map! :leader
      (:prefix-map ("j" . "doyougnu")
       (:prefix ("d" . "deft")
        :desc "Deft" "d" #'deft
        :desc "find-file" "f" #'deft-find-file
        :desc "new-file" "n" #'deft-new-file)

       (:prefix ("e" . "emacs-tools")
        :desc "Byte-compile-and-load"    "c" #'emacs-lisp-byte-compile-and-load
        :desc "Byte recompile directory" "C" #'byte-recompile-directory
        :desc "Set variable"             "s" #'set-variable
        :desc "Revert buffer"            "r" #'revert-buffer
        :desc "Highlight at point"       "h" #'highlight-symbol-at-point
        :desc "Remove Highlight"         "H" #'unhighlight-regexp
        )

       (:prefix ("i" . "IRC")
        :desc "Init" "i" #'=irc))

      (:prefix-map ("r" . "kill-ring")
       :desc "Kill-ring" "y" #'counsel-yank-pop)

      (:prefix-map ("S" . "spelling")
       :desc "Correct-word-at-point" "s" #'ispell-word
       :desc "Correct buffer"        "b" #'ispell-buffer))

(map! :map company-active-map
      "C-l" #'company-complete-selection
      "C-;" #'fill-paragraph)
