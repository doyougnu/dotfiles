;;; dyg/+dyg/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun dyg/newline-above ()
  (interactive)
  (save-excursion
    (evil-end-of-line)
    (evil-insert-newline-above)))

;;;###autoload
(defun dyg/newline-below ()
  (interactive)
  (save-excursion
    (evil-end-of-line)
    (evil-insert-newline-below)))

;;;###autoload
(defun dyg/word-insert ()
  (interactive)
  (let ((input (read-from-minibuffer "Input: ")))
    (save-excursion
      (insert input))))

;;;###autoload
(defun dyg/char-insert ()
  (interactive)
  (save-excursion
    (insert (read-char))))

;;;###autoload
(defun dyg/char-insert-after ()
  (interactive)
  (save-excursion
    (forward-char)
    (insert (read-char))))

;;;###autoload
(defun dyg/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

;;;###autoload
(defun dyg/evil-avy-line-recenter (&rest args)
  "Advice function to recenter buffer after evilavy-goto-line-above
or below."
  (recenter))

;;;###autoload
(defun dyg/shift-word-backwards ()
 "Swap the word at point with the previous word."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (when word
      (kill-word 1)
      (backward-word 2)
      (yank)
      (forward-word 1))))

;;;###autoload
(defun dyg/shift-word-forwards ()
  "Swap the word at point with the next word."
  (interactive)
  (let ((current-word (thing-at-point 'word)))
    (when current-word
      (kill-word 1)            ;; Kill the current word
      (forward-word 1)         ;; Move to the next word
      (yank)                   ;; Paste the killed word
      (backward-word 1)        ;; Move back to the next word (now in the current position)
      (kill-word 1)            ;; Kill the next word
      (backward-word 1)        ;; Move to the previous word (which was just inserted)
      (yank))))                ;; Paste the killed next word
