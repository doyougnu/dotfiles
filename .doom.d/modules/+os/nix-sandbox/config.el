;;; +os/nix-sandbox/config.el -*- lexical-binding: t; -*-

(use-package! nix-sandbox
  :defer t
  :init
  (progn
    (setq flycheck-command-wrapper-function
          (lambda (c)
            (apply 'nix-shell-command (nix-current-sandbox) c))

          flycheck-executable-find
          (lambda (c)
            (nix-executable-find (nix-current-sandbox) c)))

    (when (featurep! :lang haskell +lsp)
      (setq default-nix-wrapper
          (lambda (args)
            (append
             (append (list "nix-shell" "--command")
                     (list (mapconcat 'identity args " ")))
             (list (nix-current-sandbox))))

          haskell-process-wrapper-function
          (lambda (args)
            (apply 'nix-shell-command (nix-current-sandbox) args))

          ;; lsp variables
          lsp-haskell-server-wrapper-function default-nix-wrapper
          lsp-haskell-server-args '()))))
