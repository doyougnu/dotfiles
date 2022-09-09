;;; +editor/corfu/config.el -*- lexical-binding: t; -*-

(use-package! corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :defer t
  :hook ((common-lisp-mode . corfu-mode)
         (shell-mode       . corfu-mode)
         (eshell-mode      . corfu-mode)
         (prog-mode        . corfu-mode))
  :config
  (setq corfu-auto t)
  (advice-add #'corfu-insert :after #'corfu-send-shell)
  (map! :map eshell-mode-map
        "C-l" nil)
  (map! :map corfu-map
        :i "C-l" #'corfu-insert
        :in "TAB"   nil
        :in "RET"   nil
        "TAB"   nil
        "RET"   nil
        :in "<tab>" nil
        "<tab>" nil)

  :init
  (global-corfu-mode))
