;;; +lang/forth/config.el -*- lexical-binding: t; -*-

(use-package! forth-mode
  :defer t
  :config
  (set-repl-handler! 'forth-mode 'forth-mode-switch-to-output-buffer)
  (set-lookup-handlers! 'forth-mode
    :documentation #'forth-spec-lookup-2012)
  (setq forth-executable "gforth")
  (map! :after forth-mode
        :map forth-mode-map
        :localleader
        "," #'run-forth
        "." #'forth-eval-last-expression-display-output
        "u" #'forth-eval-last-expression
        "'" #'forth-load-file
        "r" #'forth-eval-region
        "s" #'forth-see
        "S" #'forth-switch-to-output-buffer
        "e" #'forth-eval-defun
        "R" #'forth-restart
        "H" #'forth-spec-lookup-1994
        "h" #'forth-spec-lookup-2012
        ))
