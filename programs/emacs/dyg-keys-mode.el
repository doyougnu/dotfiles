;;; dyg-keys-mode.el --- Personal keybinding minor mode -*- lexical-binding: t; -*-

;;; Code:

;; -----------------------
;; Helper functions
;; -----------------------

(defun dyg|org-clock-in-from-history ()
  "Clock into one of the recent tasks in `org-clock-history`."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-clock-in)))

(defun dyg|avy-goto-char-2 ()
  "Run `avy-goto-char-2`."
  (interactive)
  (call-interactively #'avy-goto-char-2))

(defun dyg|find-eshell-buffer ()
  (cl-find-if (lambda (buffer)
                (with-current-buffer buffer
                  (eq major-mode 'eshell-mode)))
              (buffer-list)))

(defun dyg|toggle-eshell-window ()
  (interactive)
  (if-let ((buffer (dyg|find-eshell-buffer)))
      (if (get-buffer-window buffer 'visible)
          (delete-windows-on buffer)
        (display-buffer buffer))
    (project-eshell)))

(defun dyg|recompile ()
  (interactive)
  (let ((buffer next-error-last-buffer))
    (if (and (boundp 'next-error-last-buffer)
             (buffer-live-p buffer))
        (with-current-buffer buffer (recompile))
      (call-interactively #'recompile))))

(defun dyg|toggle-compilation-window ()
  (interactive)
  (when-let ((buffer next-error-last-buffer))
    (if (get-buffer-window buffer 'visible)
        (delete-windows-on buffer)
      (display-buffer buffer))))

(defun dyg|newline ()
  (interactive)
  (save-excursion
    (end-of-line)
    (electric-newline-and-maybe-indent)))

(defun dyg|join-line ()
  (interactive)
  (save-excursion
    (forward-line 1)
    (join-line)
    (indent-according-to-mode)))

(defun dyg/paste-from-clipboard ()
  (interactive)
  (insert
   (string-trim-right
    (cond
     ((getenv "WSLENV")
      (shell-command-to-string "powershell.exe -command Get-Clipboard"))
     ((executable-find "xclip")
      (shell-command-to-string "xclip -selection clipboard -o"))
     ((eq system-type 'windows-nt)
      (shell-command-to-string "powershell.exe -command Get-Clipboard"))))))

;; -----------------------
;; Prefix keymaps
;; -----------------------

(defvar dyg/magit-map
  (let ((map (make-sparse-keymap "+magit")))
    (define-key map (kbd "v") #'magit-status)
    (define-key map (kbd "b") #'magit-blame)
    (define-key map (kbd "h") #'magit-status-here)
    (define-key map (kbd "p") #'diff-hl-show-hunk-previous)
    (define-key map (kbd "n") #'diff-hl-show-hunk-next)
    (define-key map (kbd "m") #'git-timemachine-toggle)
    (define-key map (kbd "t") #'magit-todos-list)
    (define-key map (kbd "l") #'magit-log)
    map))

(defvar dyg/buffer-map
  (let ((map (make-sparse-keymap "+buffer")))
    (define-key map (kbd "b") #'project-switch-to-buffer)
    (define-key map (kbd "l") #'project-list-buffers)
    (define-key map (kbd "i") #'ibuffer)
    (define-key map (kbd "s") #'save-buffer)
    (define-key map (kbd "r") #'revert-buffer)
    (define-key map (kbd "d") #'kill-current-buffer)
    (define-key map (kbd "x") #'scratch-buffer)
    map))

(defvar dyg/notes-map
  (let ((map (make-sparse-keymap "+notes")))
    (define-key map (kbd "s") #'org-roam-db-sync)
    (define-key map (kbd "d") #'org-roam-dailies-capture-today)
    (define-key map (kbd "'") #'org-roam-dailies-goto-today)
    (define-key map (kbd "y") #'org-roam-dailies-goto-yesterday)
    (define-key map (kbd "i") #'org-roam-node-insert)
    (define-key map (kbd "f") #'org-roam-node-find)
    (define-key map (kbd "r") #'org-roam-buffer-display-dedicated)
    (define-key map (kbd "n") #'org-add-note)
    (define-key map (kbd "C") #'org-clock-out)
    (define-key map (kbd "c") #'dyg|org-clock-in-from-history)
    map))

(defvar dyg/error-map
  (let ((map (make-sparse-keymap "+error")))
    (define-key map (kbd "n") #'next-error)
    (define-key map (kbd "p") #'previous-error)
    (define-key map (kbd "N") #'flymake-goto-next-error)
    (define-key map (kbd "P") #'flymake-goto-prev-error)
    map))

(defvar dyg/smerge-map
  (let ((map (make-sparse-keymap "+smerge")))
    (define-key map (kbd "n") #'smerge-next)
    (define-key map (kbd "p") #'smerge-prev)
    (define-key map (kbd "u") #'magit-smerge-keep-upper)
    (define-key map (kbd "l") #'magit-smerge-keep-lower)
    map))

;; -----------------------
;; Minor mode
;; -----------------------


(defvar dyg-keys-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Prefixes
    (define-key map (kbd "C-c v") dyg/magit-map)
    (define-key map (kbd "C-c b") dyg/buffer-map)
    (define-key map (kbd "C-c n") dyg/notes-map)
    (define-key map (kbd "C-c e") dyg/error-map)
    (define-key map (kbd "C-c i") dyg/smerge-map)

    ;; Globals
    (define-key map (kbd "C-t")     #'dyg|avy-goto-char-2)
    (define-key map (kbd "C-c c")   #'dyg|recompile)
    (define-key map (kbd "C-c C")   #'dyg|toggle-compilation-window)
    (define-key map (kbd "C-c o")   #'dyg|toggle-eshell-window)
    (define-key map (kbd "C-c j")   #'org-roam-dailies-goto-today)
    (define-key map (kbd "C-c a")   #'org-agenda)
    (define-key map (kbd "C-c SPC") #'org-capture)

    ;; refinements
    (define-key map (kbd "M-j")   #'dyg|join-line)
    (define-key map (kbd "C-j")   #'dyg|newline)
    (define-key map (kbd "M-t")   #'mark-sexp)
    (define-key map (kbd "M-n")   #'mark-defun)
    (define-key map (kbd "M-;")   #'comment-line
    (define-key map (kbd "C-;")   #'indent-relative))

    map)
  "Keymap for `dyg-keys-mode`.")

;; TODO: fix the +prefix in which key
;; (which-key-add-keymap-based-replacements dyg-keys-mode-map
;;   "C-c v" '("+magit"  . dyg/magit-map)
;;   "C-c b" '("+buffer" . dyg/buffer-map)
;;   "C-c n" '("+notes"  . dyg/notes-map)
;;   "C-c e" '("+error"  . dyg/error-map)
;;   "C-c i" '("+smerge" . dyg/smerge-map))

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (dyg-keys-mode -1)))
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (dyg-keys-mode 1)))

;;;###autoload
(define-minor-mode dyg-keys-mode
  "Personal minor mode replacing Meow leader bindings."
  :lighter " dyg"
  :keymap dyg-keys-mode-map
  :group 'dyg
  :global t)

(provide 'dyg-keys-mode)
;;; dyg-keys-mode.el ends here
