;;; +lang/+latex/config.el -*- lexical-binding: t; -*-

(after! latex

  ;;;;;;;;;;;;;;;;;;;;;;;;; Custom key binds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (map!
   (:map (latex-mode-map LaTeX-mode-map)
    :localleader
    "e" #'LaTeX-environment
    "E" #'LaTeX-close-environment)))
