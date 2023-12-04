;; -*- no-byte-compile: t; -*-
;;; +lang/forth/packages.el

(package! forth-mode
  :recipe (:host github
           :repo "forthy42/gforth"
           :files ("gforth.el")))
