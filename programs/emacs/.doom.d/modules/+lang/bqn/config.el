;;; +lang/bqn/config.el -*- lexical-binding: t; -*-

(use-package! bqn-mode
  :defer t
  :config
  ;; hack until bqn-mode is fixed
  (remove-hook 'bqn-inferior-mode-hook 'bqn-init)

  ;; keybinds
  (map! :map bqn--mode-map
        :localleader
        "'" #'run-bqn
        "," #'bqn/set-input-method
        (:prefix ("h" . "Help")
         :desc "Show Keyboard" "k"  #'bqn-show-keyboard
         :desc "Close Keyboard" "K" #'bqn-keymap-mode-kill-buffer
         :desc "Doc at point" "h"   #'bqn-help-symbol-info-at-point)

        (:prefix ("e" . "Eval")
         :desc "Evaluate Line"   "l" #'bqn-process-execute-line
         :desc "Evaluate Buffer" "b" #'bqn-process-execute-buffer
         :desc "Evaluate Region" "r" #'bqn-process-execute-region
         :desc "Evaluate Line and follow"   "L" #'bqn-process-execute-line-and-follow
         :desc "Evaluate Buffer and follow" "B" #'bqn-process-execute-buffer-and-follow
         :desc "Evaluate Region and follow" "R" #'bqn-process-execute-region-and-follow))

  (map!
   :map bqn-help-documentation-mode-map
   :nv "q" #'bqn/bqn-close-doc-buffer))
