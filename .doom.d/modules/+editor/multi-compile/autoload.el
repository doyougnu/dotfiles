;;; +editor/multi-compile/autoload.el -*- lexical-binding: t; -*-


;;;###autoload
(defun string/starts-with (string prefix)
    "Return t if STRING starts with prefix."
    (and (stringp string)
         (string-match (rx-to-string `(: bos ,prefix) t) string)))

;;;###autoload
(defun in-nix (args)
  (apply 'nix-shell-command (nix-current-sandbox) args))
