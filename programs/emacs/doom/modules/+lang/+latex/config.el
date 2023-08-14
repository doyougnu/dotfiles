;;; +lang/+latex/config.el -*- lexical-binding: t; -*-

(after! latex

  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-debug-bad-boxes t)

  (add-hook! LaTeX-mode-hook 'prettify-symbols-mode)
  (add-hook! latex-mode-hook 'prettify-symbols-mode)

  ;;;;;;;;;;;;;;;;;;;;;;;;; Custom key binds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (map!
   (:map (latex-mode-map LaTeX-mode-map)
    :localleader
    "i" #'LaTeX-insert-item
    "e" #'LaTeX-environment
    "E" #'LaTeX-close-environment
    "o" #'TeX-fold-paragraph
    "O" #'TeX-fold-clearout-paragraph

    "=e" #'LaTeX-fill-environment
    "=p" #'LaTeX-fill-paragraph
    "=s" #'LaTeX-fill-section
    "=n" #'LaTeX-fill-newline
    )))
