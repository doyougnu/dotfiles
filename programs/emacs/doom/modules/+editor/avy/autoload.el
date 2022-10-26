;;; +editor/avy/autoload.el -*- lexical-binding: t; -*-


;;;###autoload
(defun avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

;;;###autoload
(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun avy-action-delete (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (let* ((coords (bounds-of-thing-at-point 'word))
               (beg    (car coords))
               (end    (cdr coords)))
          (evil-delete beg end)))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun avy-action-change (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (let* ((coords (bounds-of-thing-at-point 'word))
               (beg    (car coords))
               (end    (cdr coords)))
          (evil-delete beg end)
          (insert (read-from-minibuffer "Replacement: "))))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun avy-action-insert (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (insert (read-from-minibuffer "Insert: ")))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)
