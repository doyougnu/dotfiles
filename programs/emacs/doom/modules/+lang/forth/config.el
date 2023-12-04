;;; +lang/forth/config.el -*- lexical-binding: t; -*-

(use-package! forth-mode
  :defer t
  :config
  (autoload 'forth-mode "gforth.el")
  (setq auto-mode-alist (cons '("\\.fs\\'" . forth-mode)
                              auto-mode-alist))
  (autoload 'forth-block-mode "gforth.el")
  (setq auto-mode-alist (cons '("\\.fb\\'" . forth-block-mode)
                              auto-mode-alist))

  (set-repl-handler! 'forth-mode 'forth-switch-to-interactive)

  (map! :after forth-mode
        :map forth-mode-map
        :localleader
        "," #'run-forth
        "." #'forth-send-paragraph-and-go
        "u" #'forth-eval-last-expression
        "'" #'forth-load-file
        "r" #'forth-send-region
        "t" #'forth-find-tag
        "b" #'forth-send-buffer-and-go
        "en" #'forth-next-error
        "s" #'forth-split
        ))
