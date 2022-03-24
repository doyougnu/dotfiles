;;; +editor/multi-compile/autoload.el -*- lexical-binding: t; -*-


;;;###autoload
(defun string/starts-with (str prefix)
    "Return t if STRING starts with prefix."
    (and (stringp str) (string-match-p str prefix)))

;;;###autoload
(defun in-nix (args)
  (apply 'nix-shell-command (nix-current-sandbox) args))
