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

;; convieniences
(define-key! evil-normal-state-map (kbd "C-j") 'dyg/newline-below)
(define-key! evil-normal-state-map (kbd "C-k") 'dyg/newline-above)

(setq doom-scratch-initial-major-mode 'org-mode)

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
        :desc "Byte-compile-and-load" "c" #'emacs-lisp-byte-compile-and-load
        :desc "Set-variable"          "s" #'set-variable)

       (:prefix ("i" . "IRC")
        :desc "Init" "i" #'=irc))
      (:prefix-map ("r" . "kill-ring")
       :desc "Kill-ring" "y" #'counsel-yank-pop))

(map! :map company-active-map
       "C-l" #'company-complete-selection)
