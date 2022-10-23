;;; +editor/avy/config.el -*- lexical-binding: t; -*-

(after! avy
  (setf (alist-get ?h avy-dispatch-alist) 'avy-action-helpful
        (alist-get ?. avy-dispatch-alist) 'avy-action-embark
        (alist-get ?c avy-dispatch-alist) 'avy-action-change
        (alist-get ?x avy-dispatch-alist) 'avy-action-delete))
