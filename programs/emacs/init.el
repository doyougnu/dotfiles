;; based on https://arne.me/blog/emacs-from-scratch-part-one-foundations#become-evil
;; thank you for your labor!

;; hide the ui elements
(tool-bar-mode -1)             ; Hide the outdated icons
(scroll-bar-mode -1)           ; Hide the always-visible scrollbar
(setq inhibit-splash-screen t) ; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil)      ; Ask for textual confirmation instead of GUI

;; set up straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

;; set up the garbage collection hacks
;; this has to be right after straight to reduce startup times
(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

;; a must have for nix
(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package emacs
  :init
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message ()
    (message "")))

(use-package emacs
  :init
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package emacs
  :init
  (set-charset-priority         'unicode)
  (setq locale-coding-system    'utf-8
        coding-system-for-read  'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system   'utf-8)
  (set-keyboard-coding-system   'utf-8)
  (set-selection-coding-system  'utf-8)
  (prefer-coding-system         'utf-8)
  (global-prettify-symbols-mode 1)
  (setq compilation-scroll-output t)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

(use-package emacs
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2))

(use-package emacs
  :init
  (set-face-attribute 'default nil
                      :font "Source Code Pro"
                      :height 100)
  (set-face-attribute 'fixed-pitch nil
                      :font "Source Code Pro"
                      :height 100)
  (set-face-attribute 'variable-pitch nil
                      :font "Source Code Pro"
                      :height 100))

;; themes
(setq custom-safe-themes t)
(use-package doom-themes
  :demand
  :config
(if (equal (getenv "EMACS_HOST") "framework")
    (load-theme 'doom-gruvbox-light t)
    (load-theme 'doom-laserwave     t)))

(use-package nerd-icons)

;; which-key
(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.35) ; Open after .5s instead of 1s
  :config
  (which-key-mode))

(use-package helpful
  :ensure t
  :defer
  :bind (:map global-map
         ("C-h f" . helpful-callable)
         ("C-h F" . helpful-Function)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)
         ("C-h x" . helpful-command)
         ("C-h h" . helpful-at-point)))

(use-package diff-hl
  :config
  (global-diff-hl-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emacs
  :init
  ;; allows escape to exit anything
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; hide file icon make titlebar match emacs theme
(use-package emacs
  :init
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil))

(use-package emacs
  :config
  (setq backup-directory-alist `((".*" . "~/.emacs.d/saves"))))

(use-package treesit-auto
  :defer
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package emacs
  :init
  (setq-default fill-column 80)
  (set-face-attribute 'fill-column-indicator nil
                      :foreground "#717C7C"
                      :background "transparent")
  (global-display-fill-column-indicator-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; project management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package project-tab-groups
  :ensure
  :demand
  :config
  (project-tab-groups-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; evil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use general to setup the evil keybinds
;; TODO use boon, meow, or emacs
(use-package general
  :demand
  :bind (:map global-map
              ("C-;"   . comment-line)
              ("C-M-k" . 'scroll-other-window)
              ("C-M-j" . 'scroll-other-window-down))
  :config
  (general-evil-setup)

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-.")

  (leader-keys
    "x" '(scratch-buffer :which-key "*scratch*")
    "h" '(highlight-symbol-at-point :which-key "highlight")
    "H" '(unhighlight-regexp :which-key "unhighlight")

    ;; Buffer
    "b" '(:ignore t :which-key "buffer")
    ;; this should be in consult use-package, but then I have to load consult
    ;; eagerly
    "," '(consult-buffer :which-key "fast buffer switch")
    "w" '(other-window-prefix :which-key "other-window-prefix")
    "SPC" '(org-capture :which-key "capture")

    ;; Buffers
    "b b" '(project-switch-to-buffer :which-key "switch buffer")
    "b l" '(project-list-buffers     :which-key "list buffers")
    "b s" '(save-buffer              :which-key "save buffer")
    "b r" '(revert-buffer            :which-key "revert buffer")
    ;; Don't show an error because SPC b ESC is undefined, just abort
    "b <escape>" '(keyboard-escape-quit :which-key t)
    "b d" 'kill-current-buffer

    ;; workspaces via tabs
    "t"        '(:ignore t     :which-key "tabs")
    "t n"      '(tab-new       :which-key "new tab")
    "t t"      '(tab-switch    :which-key "switch tab")
    "t d"      '(project-kill-buffers :which-key "close tabbed project")
    "t D"      '(tab-close     :which-key "kill tab")
    "t h"      '(tab-previous  :which-key "previous tab")
    "t l"      '(tab-next      :which-key "next tab")
    "t r"      '(tab-rename    :which-key "rename tab")

    ;; notes
    "n"          '(:ignore t :which-key "notes")
    "n r"        '(:ignore t :which-key "roam")
    "n r o"      '(org-roam-node-find     :which-key "find node")
    "n r i"      '(org-roam-node-insert   :which-key "insert node")
    "n r r"      '(org-roam-buffer-toggle :which-key "Toggle roam buffer")

    ;; agenda
    "a"      '(org-agenda :which-key "agenda")

    ;; smerge
    "m"        '(:ignore t :which-key "smerge")
    "m n"      '(smerge-next :which-key "smerge next")
    "m p"      '(smerge-prev :which-key "smerge prev")
    "m u"      '(smerge-upper :which-key "accept upper")
    "m l"      '(smerge-lower :which-key "accept lower")

    ;; open
    "o"          '(:ignore t :which-key "open")
    "o o"        '(find-file              :which-key "find-file-browse")
    "o O"        '(project-find-file      :which-key "find-file-search")
    "o D"        '(dired-jump             :which-key "dired")
    "o D"        '(project-find-dir       :which-key "find-dir")
    "o h"        '(find-file-other-window :which-key "find-file-other-window")

    ;; Projects
    "p"          '(:ignore t :which-key "projects")
    "p <escape>" '(keyboard-escape-quit   :which-key t)
    "p p"        '(project-switch-project :which-key "switch project")
    "p c"        '(project-compile        :which-key "compile project")
    "p w"        '(project-other-window-command :which-key "project other window")
    "p W"        '(project-other-frame-command :which-key "project other frame")
    "p d"        '(project-dired          :which-key "project dired"))

  (general-define-key
   :states '(normal visual)
    "l"   'evil-forward-word-begin
    "h"   'evil-backward-word-end
    "L"   'evil-forward-char
    "H"   'evil-backward-char
    "J"   'evil-forward-paragraph
    "K"   'evil-backward-paragraph
    "t"   'evil-find-char
    "C-j" 'evil-join
	"C-e" 'evil-jump-forward
    "C-w x" 'kill-buffer-and-window)

  (general-define-key
   :states '(normal visual)
   :keymaps 'org-mode-map
    "l"   'evil-forward-word-begin
    "h"   'evil-backward-word-end
    "L"   'evil-forward-char
    "H"   'evil-backward-char
    "J"   'evil-forward-paragraph
    "K"   'evil-backward-paragraph
    "t"   'evil-find-char
    "C-j" 'evil-join)

  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (general-define-key
               :states '(normal visual)
               :keymaps 'eglot-mode-map
               "K"   'evil-backward-paragraph)))

  ;; Stolen from (http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html)
  ;; this displays colors for escape codes in the *compilation* buffer
  (require 'ansi-color)
  (defun dyg|colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))

  (add-hook 'compilation-filter-hook #'dyg|colorize-compilation))

(use-package evil
  :demand ; No lazy loading
  :init
  ;; prevents evil and evil-collection from interfering
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t) ;; make C-u scroll up
  :config
  (evil-mode 1)
  (setq evil-undo-system 'undo-redo))

(use-package evil-lion
  :ensure t
  :bind (:map evil-normal-state-map
         ("g l" . evil-lion-left)
         ("g L" . evil-lion-right)
         :map evil-visual-state-map
         ("g l" . evil-lion-left)
         ("g L" . evil-lion-right))
  :config
  (evil-lion-mode))

(use-package evil-multiedit
  :ensure t
  :bind (:map evil-normal-state-map
		 ("g m m" . evil-multiedit-match-all)
		 ("g m j" . evil-multiedit-match-and-next)
		 ("g m k" . evil-multiedit-match-and-prev)))

(use-package magit
  :ensure t
  :demand
  :config
  (leader-keys
    "g"   '(:ignore t :which-key "magit")
    "g g" '(magit-status           :which-key "status")
    "g b" '(magit-blame            :which-key "blame")
    "g p" '(diff-hl-previous-hunk  :which-key "previous hunk")
    "g n" '(diff-hl-next-hunk      :which-key "next hunk")
    "g m" '(git-timemachine-toggle :which-key "timemachine")
    "g t" '(magit-todos-list       :which-key "todos")
    "g l" '(magit-log              :which-key "log"))

  (leader-keys
    "e"          '(:ignore t :which-key "errors")
    "e N"        '(flymake-goto-next-error :which-key "next-error")
    "e P"        '(flymake-goto-prev-error :which-key "previous-error")
    "e n"        '(next-error     :which-key "next-error")
    "e p"        '(previous-error :which-key "previous-error")))

(use-package magit-todos
  :ensure t
  :config
  (magit-todos-mode 1))
(use-package git-timemachine :ensure t)

(use-package writeroom-mode
  :ensure t
  :demand
  :config

  (leader-keys "z" '(writeroom-mode :which-key "writeroom"))
  ;; just make the write area a tad larger than wrap
  (setq writeroom-width 90)
  (setq writeroom-fullscreen-effect 'maximized)

  ;; bump font in writeroom
  (add-hook 'writeroom-mode-enable-hook (lambda () (text-scale-set 1)))
  (add-hook 'writeroom-mode-disable-hook (lambda () (text-scale-set 0))))

;; a utility package to get magit to play nice with evil
;; we can already see how much complexity is added due to evil
(use-package evil-collection
  :after evil
  :demand
  :config
  (evil-collection-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eglot, lsp-mode is slow even for rust, bad software
(use-package emacs
  :hook (zig-mode        . eglot-ensure)
  :hook (rust-mode       . eglot-ensure)
  :hook (scheme-mode     . eglot-ensure)
  :hook (haskell-mode    . eglot-ensure)
  :hook (lisp-mode       . eglot-ensure)
  :hook (typescript-mode . eglot-ensure)

  :config

  (leader-keys
   "l" '(:ignore t :which-key "lsp")
   "l <escape>" '(keyboard-escape-quit :which-key t)
   "l r" '(eglot-rename :which-key "rename")
   "l a" '(eglot-code-actions :which-key "code actions")))

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package rust-mode
  :init
  ;; TODO (setq rust-mode-treesitter-derive t)
  :hook (rust-mode . prettify-symbols-mode)
  :hook (rust-mode . display-line-numbers-mode)
  :config

  ;; Turn off for inlay hints for rust
  (add-hook 'eglot-managed-mode-hook
			(lambda ()
			  (when (derived-mode-p 'rust-mode)
				(eglot-inlay-hints-mode -1))))
  (setq rust-format-on-save t)
  (leader-keys
    "c" '(:ignore t :which-key "mode")
    "c <escape>" '(keyboard-escape-quit :which-key t)
    "c b" '(rust-compile :which-key "build")
    "c r" '(rust-run :which-key "run")
    "c t" '(rust-test :which-key "test")
    "c k" '(rust-check :which-key "check")
    "c c" '(rust-run-clippy :which-key "clippy")))

(use-package haskell-mode
  :hook (haskell-mode . haskell-indentation-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;; completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  :config
  (general-define-key
   :keymaps 'vertico-map
    "C-t"   'vertico-previous
    "C-a"   'vertico-directory-up
    "C-n"   'vertico-next
    "C-i"   'vertico-directory-enter))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  :init
  (global-corfu-mode)
  :config
  (general-define-key
   :keymaps 'corfu-map
   "C-t"   'corfu-previous
   "C-n"   'corfu-next
   "C-a"   'corfu-complete)

  (general-define-key
   :states '(insert)
   "C-t"   'corfu-previous
   "C-n"   'corfu-next
   "C-a"   'corfu-complete))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s s" . dyg|consult-ripgrep-word-at-point)
         ("M-s r" . consult-ripgrep)
         ("C-s"   . dyg|consult-line-word-at-point)
         ("M-s i" . consult-imenu-multi)
         ("M-s I" . consult-imenu)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;;;###autoload
  (defun dyg|consult-ripgrep-word-at-point ()
    "Run `consult-ripgrep` preloaded with the word under the cursor."
    (interactive)
    (let ((word (thing-at-point 'word t)))
      (if word
          (consult-ripgrep nil word)
        (consult-ripgrep nil nil))))

  ;;;###autoload
  (defun dyg|consult-line-word-at-point ()
    "Run `consult-line` preloaded with the word under the cursor."
    (interactive)
    (let ((word (thing-at-point 'word t)))
      (if word
          (consult-line-multi nil word)
        (consult-line-multi nil nil))))

  ;; avoid certain buffers in search
  (mapcar
   (lambda (rx) (add-to-list 'consult-buffer-filter rx))
   '("\\`magit\\(?:-[a-z]+\\)?:"
     "\\*Warnings\\*"
     "\\*helpful.*"
     "\\*Help\\*"
     "\\*\\#.*\\#\\*" ;; temp files
     "\\*Async-native-compile-log\\*"
     "\\*Messages\\*"
     "\\`\\*EGLOT .* events\\*\\'"))

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; avy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :bind (:map evil-normal-state-map
         ("g j " . evil-avy-goto-line-below)
         ("g k " . evil-avy-goto-line-above)
         ("M-t " . evil-avy-goto-char-2)
         :map evil-visual-state-map
         ("g j " . evil-avy-goto-line-below)
         ("g k " . evil-avy-goto-line-above)
         ("M-t " . evil-avy-goto-char-2))
  :config
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq initial-major-mode 'org-mode)

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :bind (:map org-mode-map
        ("S-<return>" . dyg|org-insert-subheading-respect-content))
  :config

  (defun dyg|org-insert-subheading-respect-content ()
  "Insert a subheading respecting the content below the current heading."
  (interactive)
  (org-insert-heading-respect-content)
  (org-do-demote))

  (defun dyg|org-latex-preview ()
	"Automatically refresh LaTeX fragments in the current buffer."
	(when (eq major-mode 'org-mode)
	  (org-latex-preview)))

  ;; Enable automatic LaTeX fragment preview
  (defun dyg|org-latex-preview-setup ()
  "Set up auto LaTeX fragment preview."
  (add-hook 'after-save-hook #'dyg|org-latex-preview nil t))

  (add-hook 'org-mode-hook #'dyg|org-latex-preview-setup)

  (setq org-startup-with-inline-images t)
  (setq org-M-RET-may-split-line nil)
  (setq org-startup-indented t)
  (setq org-format-latex-options
		(plist-put org-format-latex-options :scale 1.5))
  (add-hook 'org-mode-hook
            #'(lambda ()
                (add-hook 'evil-insert-state-entry-hook
                          #'(lambda () (setq-local org-hide-emphasis-markers nil)))
                (add-hook 'evil-insert-state-exit-hook
                          #'(lambda () (setq-local org-hide-emphasis-markers t)))))


  ;; from https://github.com/alphapapa/unpackaged.el?tab=readme-ov-file#ensure-blank-lines-between-headings-and-before-contents
  (defun dyg|org-fix-blank-lines ()
    "Ensure that blank lines exist between headings and between
    headings and their contents. Operates on whole buffer."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (org-map-entries (lambda ()
                         (org-with-wide-buffer
                          ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                          ;; newlines before the current heading, so we do this part widened.
                          (while (not (looking-back "\n\n" nil))
                            ;; Insert blank lines before heading.
                            (insert "\n")))
                         (let ((end (org-entry-end-position)))
                           ;; Insert blank lines before entry content
                           (forward-line)
                           (while (and (org-at-planning-p)
                                       (< (point) (point-max)))
                             ;; Skip planning lines
                             (forward-line))
                           (while (re-search-forward org-drawer-regexp end t)
                             ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                             ;; for some reason it doesn't work correctly when operating on hidden text.
                             ;; This works, taken from `org-agenda-get-some-entry-text'.
                             (re-search-forward "^[ \t]*:END:.*\n?" end t)
                             (goto-char (match-end 0)))
                           (unless (or (= (point) (point-max))
                                       (org-at-heading-p)
                                       (looking-at-p "\n"))
                             (insert "\n"))))
                       t nil)))

  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'before-save-hook  #'dyg|org-fix-blank-lines)

  (setq org-directory       "~/sync/org")
  (setq org-clock-idle-time 20)
  (setq org-pretty-entities t)
  (setq org-clock-persist   'history)
  (setq org-journal-file-format                "%Y/%m/%d.org")
  (setq org-columns-default-format             "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-carryover-items           "TODO=\"NEXT\"|TODO=\"TODO\"|TODO=\"HOLD\"|TODO=\"INPROG\"")
  (setq org-confirm-babel-evaluate             nil)
  (setq org-edit-src-content-indentation      2)
  ;; don't set bookmarks on a capture
  (setf org-bookmark-names-plist             nil)
  ;; request a note everytime we clock out on clocked in item
  (setf org-log-note-clock-out           t)
  (org-clock-persistence-insinuate)

  ;; load my personal org agenda and capture config
  (load-user-file "personal-org.el")

  ;; use firefox
  (setf browse-url-browser-function 'browse-url-firefox))

(use-package evil-org
  :ensure t
  :after org
  :hook ((org-mode . evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-bullets
  :ensure t
  :after evil-org
  :hook ((org-mode . org-bullets-mode)
         (evil-org-mode . org-bullets-mode)))

(use-package org-roam
  :ensure t
  :config

  (setq org-roam-directory (file-truename "~/sync/roam")
        org-roam-inbox     (concat org-roam-directory "/" "refile.org"))

  (setq org-roam-capture-templates
        '(("m" "main" plain "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("l" "literature" plain "%?"
           :if-new
           (file+head "literature/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("c" "concepts" plain "%?"
           :if-new
           (file+head "concepts/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t)))

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  ;;;###autoload
  (defun roam|tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))

  (add-hook 'org-roam-capture-new-node-hook #'roam|tag-new-node-as-draft))

(use-package yasnippet
  :after evil-org
  :ensure t
  :demand
  :bind* ("M-o" . yas-expand) ;; Global binding for yas-expand
  :config
  (use-package yasnippet-snippets :ensure t)
  (setq yas-snippet-dirs
        '("/home/doyougnu/.emacs.d/snippets/"))
  (yas-global-mode 1)
  ;; (define-key yas-minor-mode-map (kbd "M-o") #'yas-expand)
  (define-key yas-minor-mode-map (kbd "M-o") #'yas-next-field-or-maybe-expand)
  (define-key yas-keymap         (kbd "M-o") #'yas-next-field-or-maybe-expand)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;; globals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq global-display-line-numbers-mode t)
(setq whitespace-style '(face spaces trailing tabs space-mark tab-mark))

(use-package emacs
  :init
  (if (not (equal (getenv "EMACS_HOST") "framework"))
      (with-eval-after-load 'whitespace-mode
        (set-face-attribute 'whitespace-space nil
                            :foreground "gray18")
        (set-face-attribute 'whitespace-tab nil
                            :foreground "gray18")))
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (setq global-whitespace-mode 1))
