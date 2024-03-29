;;; dyg/+cl/config.el -*- lexical-binding: t; -*-


(use-package! sly-asdf
  :defer t

  :init
  (add-to-list 'sly-contribs 'sly-asdf 'append)

  :config
  (map! :map (sly-mode-map sly--completion-transient-mode-map sly--completion-display-map)
        :i "C-k" #'sly-prev-completion
        :i "C-j" #'sly-next-completion
        :i "C-l" #'sly-choose-completion)

  (add-hook! 'sly-mode-hook #'auto-fill-mode)

  (setq doom-localleader-key ","
        doom-localleader-alt-key "C-."
        ;; inferior-lisp-program "nix develop -c sbcl"
        common-lisp-hyperspec-root (expand-file-name "~/programming/cl/HyperSpec/")))
