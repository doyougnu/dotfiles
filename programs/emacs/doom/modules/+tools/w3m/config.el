;;; +tools/w3m/config.el -*- lexical-binding: t; -*-

(use-package! w3m
  :defer t
  :commands (w3m)
  :config
  (when (modulep! :editor evil +everywhere)
    (evil-collection-w3m-setup))
  ;; (setq browse-url-browser-function 'w3m-browse-url)
  )
