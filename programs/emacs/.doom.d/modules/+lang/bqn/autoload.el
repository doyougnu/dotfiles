;;; +lang/bqn/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun bqn/set-input-method ()
  (interactive)
  (toggle-input-method "BQN-Z"))

;;;###autoload
(defun bqn/bqn-close-doc-buffer ()
  (interactive)
  (bqn-help--close-documentation-buffer))
