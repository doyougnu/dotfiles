;;; +editor/symex/config.el -*- lexical-binding: t; -*-

(use-package! symex
  :defer t
  ;; only load this package once sly has been called
  :after-call sly
  :config
  (symex-initialize)
  (setq browse-url-browser-function 'w3m-browse-url)
  ;; have to unbind set input-mode
  (map! :map global-map
        "C-\\" nil)
  ;; Now add \ to start, and C-\ to return to normal evil state. Both of these
  ;; are added to 'normal and 'visual mode
  (map! :map symex-mode-map
        :desc "Symex start!" :nv "\\" #'symex-mode-interface
        :desc "Symex stop!"  :nvi  "C-\\" #'evil-normallike-state
        :nv "C-." #'symex-shift-forward
        :nv "C-," #'symex-shift-backward)
  (map! :map evil-motion-state-map
        :desc "Symex start!" "\\" #'symex-mode-interface)
  (map! :map evil-insert-state-map
        :desc "Symex start!" "C-f" #'symex-mode-interface)

  ;; This hook returns us to symex upon returning to normal mode. Yes this means
  ;; your normal mode becomes symex
  ;; (add-hook! 'evil-normal-state-entry-hook #'symex-mode-interface t)
  ;; These hooks ensure symex doesn't bleed into other modes where it doesn't
  ;; make sense, although I've found it to be quite good at editing org-mode
  ;; surprisingly!
  ;; (add-hook! 'helpful-mode-hook            #'evil-normallike-state)
  ;; (add-hook! 'sly-xref-mode-hook           #'evil-normallike-state)
  ;; (add-hook! 'sly-apropos-mode-hook        #'evil-normallike-state)

  :custom
  (symex-common-lisp-backend 'sly)
  (symex-modal-backend 'evil))
