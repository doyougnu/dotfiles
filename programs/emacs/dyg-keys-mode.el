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

;;; Text Object Selection Configuration

(defun dyg/get-bounds-of-thing (type)
  "Return the (start . end) bounds of TYPE around point.
TYPE can be: 'paren, 'bracket, 'curly, 'string, 'defun, 'line, 'buffer."
  (save-excursion
    (let ((orig-point (point)))
      (condition-case nil
          (cond
           ;; 1. ROUND PARENTHESES ()
           ((eq type 'paren)
            (let ((start (scan-lists (point) -1 1)))
              (cons start (scan-lists start 1 0))))

           ;; 2. SQUARE BRACKETS []
           ((eq type 'bracket)
            ;; Search backward for [ escaping strings/comments, limited logic
            (search-backward "[" nil t)
            (let ((start (point)))
              (forward-list)
              (cons start (point))))

           ;; 3. CURLY BRACES {}
           ((eq type 'curly)
            (search-backward "{" nil t)
            (let ((start (point)))
              (forward-list)
              (cons start (point))))

           ;; 4. STRINGS ""
           ((eq type 'string)
            (let ((syntax (syntax-ppss)))
              (if (nth 3 syntax) ;; If inside string
                  (let* ((start (nth 8 syntax))
                         (end (save-excursion
                                (goto-char start)
                                (forward-sexp)
                                (point))))
                    (cons start end))
                ;; Attempt to find nearby string if not strictly inside
                (search-backward "\"" (line-beginning-position) t)
                (let ((start (point)))
                  (forward-sexp)
                  (cons start (point))))))

           ;; 5. FUNCTIONS (Defun)
           ((eq type 'defun)
            (end-of-defun)
            (let ((end (point)))
              (beginning-of-defun)
              (cons (point) end)))

           ;; 6. LINE
           ((eq type 'line)
            (cons (line-beginning-position) (line-end-position)))

           ;; 7. BUFFER
           ((eq type 'buffer)
            (cons (point-min) (point-max))))

        (error nil)))))

(defun dyg/select-text-object (type inner)
  "Mark the object TYPE. If INNER is non-nil, exclude delimiters."
  (let ((bounds (dyg/get-bounds-of-thing type)))
    (if bounds
        (progn
          (push-mark (car bounds) t t)
          (goto-char (cdr bounds))

          ;; Adjust for Inner Selection
          (when inner
            (cond
             ;; For delimiters (parens, brackets, strings), shrink by 1 char
             ((memq type '(paren bracket curly string))
              (if (> (point) (mark))
                  (progn (backward-char 1) (exchange-point-and-mark) (forward-char 1))
                (progn (forward-char 1) (exchange-point-and-mark) (backward-char 1))))
             ;; For line, usually 'inner' means trim whitespace (optional preference)
             ((eq type 'line)
              ;; Simple trim implementation could go here
              nil)))

          ;; Activate the region visually
          (setq deactivate-mark nil)
          (message "Marked %s %s" (if inner "inner" "outer") type))
      (message "No %s found around point." type))))

;;; Dispatchers

(defvar dyg/text-object-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") (lambda () (interactive) (dyg/select-text-object 'paren nil)))
    (define-key map (kbd "(") (lambda () (interactive) (dyg/select-text-object 'paren nil)))
    (define-key map (kbd ")") (lambda () (interactive) (dyg/select-text-object 'paren nil)))

    (define-key map (kbd "b") (lambda () (interactive) (dyg/select-text-object 'bracket nil)))
    (define-key map (kbd "[") (lambda () (interactive) (dyg/select-text-object 'bracket nil)))
    (define-key map (kbd "]") (lambda () (interactive) (dyg/select-text-object 'bracket nil)))

    (define-key map (kbd "B") (lambda () (interactive) (dyg/select-text-object 'curly nil)))
    (define-key map (kbd "{") (lambda () (interactive) (dyg/select-text-object 'curly nil)))
    (define-key map (kbd "}") (lambda () (interactive) (dyg/select-text-object 'curly nil)))

    (define-key map (kbd "s") (lambda () (interactive) (dyg/select-text-object 'string nil)))
    (define-key map (kbd "\"") (lambda () (interactive) (dyg/select-text-object 'string nil)))

    (define-key map (kbd "f") (lambda () (interactive) (dyg/select-text-object 'defun nil)))
    (define-key map (kbd "l") (lambda () (interactive) (dyg/select-text-object 'line nil)))
    (define-key map (kbd "g") (lambda () (interactive) (dyg/select-text-object 'buffer nil)))
    map)
  "Map for selecting text objects.")

(defun dyg/dispatch-mark-inner ()
  "Dispatch key for selecting Inner text objects."
  (interactive)
  (message "Inner: (p)aren (b)racket {B}curly (s)tring (f)unc (l)ine (g)lobal")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     ;; Map specific keys to call the selector with INNER = true
     (define-key map (kbd "p") (lambda () (interactive) (dyg/select-text-object 'paren t)))
     (define-key map (kbd "(") (lambda () (interactive) (dyg/select-text-object 'paren t)))
     (define-key map (kbd "b") (lambda () (interactive) (dyg/select-text-object 'bracket t)))
     (define-key map (kbd "[") (lambda () (interactive) (dyg/select-text-object 'bracket t)))
     (define-key map (kbd "B") (lambda () (interactive) (dyg/select-text-object 'curly t)))
     (define-key map (kbd "{") (lambda () (interactive) (dyg/select-text-object 'curly t)))
     (define-key map (kbd "s") (lambda () (interactive) (dyg/select-text-object 'string t)))
     (define-key map (kbd "f") (lambda () (interactive) (dyg/select-text-object 'defun t)))
     (define-key map (kbd "l") (lambda () (interactive) (dyg/select-text-object 'line t)))
     (define-key map (kbd "g") (lambda () (interactive) (dyg/select-text-object 'buffer t)))
     map)))

(defun dyg/dispatch-mark-outer ()
  "Dispatch key for selecting Outer text objects."
  (interactive)
  (message "Outer: (p)aren (b)racket {B}curly (s)tring (f)unc (l)ine (g)lobal")
  (set-transient-map dyg/text-object-map))

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
    (define-key map (kbd "M-j") #'dyg|join-line)
    (define-key map (kbd "C-j") #'dyg|newline)
    (define-key map (kbd "M-t") #'mark-sexp)
    (define-key map (kbd "M-n") #'mark-defun)
    (define-key map (kbd "M-;") #'comment-line)
    (define-key map (kbd "C-;") #'indent-relative)
    (define-key map (kbd "C-,") #'dyg/dispatch-mark-outer)
    (define-key map (kbd "C-.") #'dyg/dispatch-mark-inner)

    map)
  "Keymap for `dyg-keys-mode`.")


;; tweak org-mode general keybinds
(define-key org-mode-map (kbd "M-RET") #'dyg|org-insert-subheading-respect-content)

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
