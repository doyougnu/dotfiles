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
