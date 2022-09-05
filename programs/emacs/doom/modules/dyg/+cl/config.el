;;; dyg/+cl/config.el -*- lexical-binding: t; -*-


(use-package! sly-asdf
  :defer t

  :init
  (add-to-list 'sly-contribs 'sly-asdf 'append)

  (map! :map (sly-mode-map sly--completion-transient-mode-map sly--completion-display-map)
        :nvi "C-k" #'sly-prev-completion
        :nvi "C-j" #'sly-next-completion
        :nvi "C-l" #'sly-choose-completion)

  (setq doom-localleader-key ","
        doom-localleader-alt-key "C-."
        inferior-lisp-program "nix develop -c sbcl"
        common-lisp-hyperspec-root (expand-file-name "~/programming/cl/HyperSpec/")))
