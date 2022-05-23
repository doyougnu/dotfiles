;;; dyg/+ispell/config.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix-map ("j" . "doyougnu")
       (:prefix ("s" . "spell")
        :desc "Check word"   "s" #'ispell-word
        :desc "Check buffer" "b" #'ispell)))
