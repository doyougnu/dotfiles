;;; dyg/+dyg/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun dyg/newline-above ()
  (interactive)
  (evil-end-of-line)
  (evil-insert-newline-above))

;;;###autoload
(defun dyg/newline-below ()
  (interactive)
  (evil-end-of-line)
  (evil-insert-newline-below))

;;;###autoload
(defun dyg/insert ()
  (interactive)
  (let ((input (read-from-minibuffer "Input: ")))
    (save-excursion
      (insert input))))
