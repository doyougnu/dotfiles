;;; +lang/klister/config.el -*- lexical-binding: t; -*-

(use-package! klister-mode
  :defer t
  :init
  (map! :after klister
        :map klister-mode-map
        :localleader
        "'" #'klister-run-current-buffer
        "," #'klister-run-file
        "f" #'klister-run-buffer))
