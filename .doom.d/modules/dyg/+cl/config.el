;;; dyg/+cl/config.el -*- lexical-binding: t; -*-


(use-package! sly-asdf
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-asdf 'append))
