;;; +lang/+haskell/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +haskell/haskell-interactive-bring ()
  "Bring the inferior haskell process for this session but don't switch to it"
  (interactive)
  (haskell-session-buffer (haskell-session)))
