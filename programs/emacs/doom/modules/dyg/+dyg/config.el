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
(map! (:map (lispy-mode-map global-mode-map) "C-t" nil)
      (:map (evil-normal-state-map lispy-mode-map)
       (:prefix-map ("C-t" . "easy-insert")
        "i" #'dyg/char-insert
        "a" #'dyg/char-insert-after
        "w" #'dyg/word-insert
        "j" #'dyg/newline-below
        "k" #'dyg/newline-above)))

(setq doom-scratch-initial-major-mode 'org-mode)

;; find the nearest nix file
;; (defun projectile-nix-root-dir (dir)
;;   "Retrieve the root DIR based on nix files"
;;   (let ((default-directory dir)
;;         (current-path      (nix-current-sandbox)))
;;     (if current-path
;;         (file-name-directory current-path)
;;         nil)))

;; set projectile to recently used and enable caching
(setq projectile-enable-caching t
      projectile-sort-order      'recently-active
      projectile-indexing-method 'hybrid
      projectile-project-root-functions '(projectile-root-bottom-up
                                          projectile-root-local
                                          projectile-root-top-down
                                          projectile-root-top-down-recurring))

;; always prefer newer .el files
(setq load-prefer-newer t)

;; use the text browser
;; (setq +lookup-open-url-fn #'eww)

;; faster which-keys
(setq which-key-idle-delay 0.1)

;; show whitespace
(setq-default show-trailing-whitespace 'trailing)

;; better docs from dash
(setq dash-docs-common-docsets '("Haskell" "Emacs Lisp" "Common Lisp"))

;; set evil to not move the cursor when exiting insert mode
(setq evil-move-cursor-back nil)

;; recentering hooks and advice
(add-hook! 'better-jumper-pre-jump-hook :append #'recenter)
(add-hook! 'better-jumper-post-jump-hook :append #'recenter)
(advice-add 'evil-avy-goto-line-above :after #'dyg/evil-avy-line-recenter)
(advice-add 'evil-avy-goto-line-below :after #'dyg/evil-avy-line-recenter)

;; setup EasyPG for gpg
(setq epg-pinentry-mode 'loopback)

;; you so pretty
(setq global-prettify-symbols-mode t)

;; set geiser default
(setq geiser-default-implementation 'chez)

;; deft setup
(setq deft-directory "/home/doyougnu/sync/deft")
(setq deft-use-filename-as-title t)
(setq deft-recursive t)

(with-eval-after-load 'biblatex
  (setq org-ref-default-bibliography
        (directory-files-recursively projectile-project-root "\\.bib$")))

;; set avy keys
(after! avy
  ;; home row priorities: 8 6 4 5 - - 1 2 3 7
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;; custom highlighting for todo keywords
(after! hl-todo
  (setq hl-todo-keyword-faces
        '(("HOLD"   . "#d0bf8f")
          ("DYG"    . "#d0bf8f")
          ("TODO"   . "#cc9393")
          ("NEXT"   . "#dca3a3")
          ("THEM"   . "#dc8cc3")
          ("PROG"   . "#7cb8bb")
          ("OKAY"   . "#7cb8bb")
          ("DONT"   . "#5f7f5f")
          ("FAIL"   . "#8c5353")
          ("DONE"   . "#afd8af")
          ("NOTE"   . "#d0bf8f")
          ("MAYBE"  . "#d0bf8f")
          ("HACK"   . "#d0bf8f")
          ("TEMP"   . "#d0bf8f")
          ("FIXME"  . "#cc9393")
          ("XXXX*"  . "#cc9393"))))

(map! :leader
      (:prefix-map ("i" . "insert")
                   :desc "Todo" "t" #'hl-todo-insert))

;; Yas
(setq yas-global-mode t)
(map! :map (org-mode-map evil-insert-state-map global-map)
"M-i" #'yas-expand)
(map! :map yas-minor-mode-map
"M-i" #'yas-expand)
(map! :map yas-keymap
"M-i" #'yas-next-field-or-maybe-expand)

;; be explicit about the shell
(setq shell-file-name (getenv "SHELL"))

;; workspaces
(after! doom
  (map! :map doom-leader-workspace-map
         :desc "Previous workspace"  "h"   #'+workspace/switch-left
         :desc "Next workspace"      "l"   #'+workspace/switch-right))

;; multi cursors
(after! evil-multiedit
  (map!
   :map evil-multiedit-mode-map
   :nv "N" #'evil-multiedit-match-and-next
   :nv "M" #'evil-multiedit-match-and-prev))

;; workaround emacs-everwhere DEL bug:
;; https://github.com/tecosaur/emacs-everywhere/issues/49
(setq emacs-everywhere-mode-initial-map nil)

(after! emacs-everywhere
  (map! :map emacs-everywhere-mode-map
        :nv ",," #'emacs-everywhere-finish-or-ctrl-c-ctrl-c))

(after! evil

  (map! :leader
        :desc "find file"               "oo" #'find-file
        :desc "find recent file"        "ot" #'consult-recent-file
        :desc "find file other project" "oF" #'doom/find-file-in-other-project
        :desc "find file other window"  "ow" #'projectile-find-file-other-window
        :desc "projectile: find file"   "oO" #'projectile-find-file
        :desc "ace-window"              "ww" #'ace-window)

  (map! :map (evil-normal-state-map evil-visual-state-map)
        "l" #'evil-forward-word-begin
        "h" #'evil-backward-word-begin
        "L" #'evil-forward-char
        "H" #'evil-backward-char
        "J" #'evil-forward-paragraph
        "K" #'evil-backward-paragraph
        "t" #'evil-avy-goto-char-2-below
        "T" #'evil-avy-goto-char-2-above
        "gj" #'evil-avy-goto-line-below
        "gk" #'evil-avy-goto-line-above
        "gmj" #'evil-multiedit-match-and-next
        "gmk" #'evil-multiedit-match-and-prev
        "gmr" #'evil-multiedit-match-all
        "gmm" #'evil-multiedit-match-symbol-and-next
        "C-j" #'evil-join
        "C-o" #'better-jumper-jump-backward
        "C-e" #'better-jumper-jump-forward
        "C-k" #'electric-newline-and-maybe-indent))

;; keybindings
(map! :leader
      "z" #'zone
      (:prefix-map ("d" . "doyougnu")
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

       "h" #'org-agenda)

      (:prefix ("r" . "kill-ring")
       :desc "Kill-ring" "y" #'consult-yank-pop)

      (:prefix ("S" . "spelling")
       :desc "Correct-word-at-point" "s" #'ispell-word
       :desc "Correct buffer"        "b" #'ispell-buffer)

      (:prefix ("t" . "toggle")
       :desc "debug-on-error" "d" #'toggle-debug-on-error)

      (:prefix ("s" . "search")
       :desc "Search project for point" "p" #'+vertico/project-search-from-cwd)

      (:prefix ("g" . "+git")
       :desc "next hunk" "n" #'git-gutter:next-hunk
       :desc "next hunk" "p" #'git-gutter:previous-hunk)

      (:when (modulep! :tools lsp)
       :desc "lsp-imenu" "cI" #'lsp-ui-imenu)

      (:prefix ("m" . "smerge")
       :desc "smerge-next-in-file" "n" #'smerge-next
       :desc "smerge-next"         "N" #'smerge-vc-next-conflict
       :desc "smerge-prev"         "p" #'smerge-prev
       :desc "smerge-keep-current" "m" #'smerge-keep-current
       :desc "smerge-keep-upper"   "u" #'smerge-keep-upper
       :desc "smerge-keep-lower"   "l" #'smerge-keep-lower)

      (:after doom
      :desc "Org-capture" "SPC" #'org-capture
      :desc "switch buffer other window" "'" #'projectile-switch-to-buffer-other-window
      :desc "Ranger"      "."   #'ranger))

(map! :leader
      (:prefix-map ("n" . "notes")
                   (:prefix-map ("c" . "clock")
                    :desc "clock-in" "i" #'org-mru-clock-in
                    :desc "clock-out" "o" #'org-clock-out
                    :desc "goto-clock" "g" #'org-clock-goto
                    :desc "recent-clock" "r" #'org-mru-clock-select-recent-task)

                   :desc "open refile"      "SPC"   #'dyg/org-open-default-todo-file
                   :desc "open roam refile" "r SPC" #'dyg/org-open-default-roam-file
                   :desc "Find node"  "ro" #'org-roam-node-find
                   :desc "Add Tag"    "rt" #'org-roam-tag-add
                   :desc "Remove Tag" "rT" #'org-roam-tag-remove))

(map! :map company-active-map
      :after company
      "C-s" #'company-complete-selection
      "C-t" #'company-filter-candidates
      "C-." #'fill-paragraph)

(map! (:map company-active-map
            "C-s" #'company-complete-selection
            "C-." #'fill-paragraph)
      (:map minibuffer-local-map
            "C-t" #'vertico-previous
            "C-s" #'vertico-directory-enter
            "C-e" #'embark-act)
      (:map corfu-mode-map
            "C-s" #'corfu-complete
            "C-t" #'corfu-previous
            "C-n" #'corfu-next)
      (:map evil-markdown-mode-map
       :nv "C-." #'fill-paragraph))

(map! :map global-map
       :nv "C-." #'fill-paragraph)

;; unsure if i want to keep this because I can just use C-n and C-p I'm more
;; sure now this is useful for swiper which uses C-n and C-p to move candidates,
;; so having these makes sense as a backup
(map! :map minibuffer-local-map
      "C-o" #'previous-history-element
      "C-u" #'next-history-element)


(map! :map eshell-mode-map
      :ni "C-n" #'eshell-next-matching-input-from-input
      :ni "C-p" #'eshell-previous-matching-input-from-input
      :ni "C-n" #'eshell-next-matching-input-from-input
      :ni "C-t" #'eshell-previous-matching-input-from-input)

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
(setq vertico-grid-mode 1)
(advice-add #'completing-read-multiple
            :override #'consult-completing-read-multiple)

;; force ivy to not search until 3 chars have been input
(after! ivy
  (setq ivy-more-chars-alist '((counsel-grep   . 3)
                               (counsel-rg     . 3)
                               (counsel-search . 3)
                               (t              . 3))))

(setq browse-url-browser-function 'firefox)

;;;;;;;;;;;;;;;;;;;;;;;;; Comint config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process color escape sequences
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
