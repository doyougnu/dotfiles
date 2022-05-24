;;; +lang/elisp/autoload.el -*- lexical-binding: t; -*-
;;; Most of this code comes from: https://caiorss.github.io/Emacs-Elisp-Programming/Elisp_Snippets.html#sec-2

;; see: https://emacsredux.com/blog/2013/04/29/start-command-or-switch-to-its-buffer/
;;;###autoload
(defun start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

;;;###autoload
(defun start-dont-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (let ((old-buffer (current-buffer)))
          (split-window-sensibly (selected-window))
          (other-window 1)
          (funcall function)
          (pop-to-buffer old-buffer)))
    (display-buffer buffer-name)))

;;;###autoload
(defun ielm/begin ()
  "Switch to default `ielm' buffer. Start `ielm' if it's not already running."
  (interactive)
  (start-dont-switch-to 'ielm "*ielm*"))

;;;###autoload
(defun ielm/select-buffer-other-window ()
  "Select other window' buffer as IELM's working buffer."
  (interactive)
  (let ((buf (save-window-excursion
               (other-window 1)
               (current-buffer))))
    (ielm-change-working-buffer buf)
    (ielm-print-working-buffer)))

;;;###autoload
(defun ielm/send-region ()
  (interactive)
  (let ((text (buffer-substring-no-properties (region-beginning)
                                              (region-end))))
    (with-current-buffer "*ielm*"
      (insert text)
      (ielm-send-input))))

;;;###autoload
(defun ielm/select-buffer-other-window ()
  "Select other window' buffer as IELM's working buffer."
  (interactive)
  (let ((buf (save-window-excursion
               (other-window 1)
               (current-buffer))))
    (ielm-change-working-buffer buf)
    (ielm-print-working-buffer)))

;;;###autoload
(defun ielm/clear ()
  "Clear IELM buffer."
  (interactive)
  (with-current-buffer "*ielm*"
      (let ((inhibit-read-only t))
        (erase-buffer))))

;; Idea from http://www.reddit.com/r/emacs/comments/312ge1/i_created_this_function_because_i_was_tired_of/
;; also found in spacemacs
;;;###autoload
(defun ielm/eval-current-form ()
  "Find and evaluate the current def* or set* command.
Unlike `eval-defun', this does not go to topmost function."
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))
