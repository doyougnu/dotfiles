;;; +editor/avy/config.el -*- lexical-binding: t; -*-

(after! avy
  (setf (alist-get ?h avy-dispatch-alist) 'avy-action-helpful
        (alist-get ?. avy-dispatch-alist) 'avy-action-embark
        (alist-get ?, avy-dispatch-alist) 'avy-action-ispell-word
        (alist-get ?c avy-dispatch-alist) 'avy-action-change
        (alist-get ?I avy-dispatch-alist) 'avy-action-ispell
        (alist-get ?x avy-dispatch-alist) 'avy-action-delete))
