;;; +lang/+latex/config.el -*- lexical-binding: t; -*-

(after! latex

  ;;;;;;;;;;;;;;;;;;;;;;;;; Custom key binds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (map!
   (:map latex-mode-map
    :localleader
    "e" #'LaTeX-close-environment)))
