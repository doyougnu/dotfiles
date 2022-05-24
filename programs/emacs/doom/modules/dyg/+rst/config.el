(after! rst
;;;;;;;;;;;;;;;;;;;;;;;;; Custom key binds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (map!
   (:map rst-mode-map
    :localleader
    (:prefix ("a" . "adjust")
     "a" #'rst-adjust
     "d" #'rst-display-hdr-hierarchy
     "h" #'describe-prefix-bindings
     "s" #'rst-straighten-sections)

    (:prefix ("i" . "insert")
     "l" #'rst-insert-list
     "b" #'rst-line-block-region)

    (:prefix ("c" . "compile")
     "c" #'rst-compile
     "a" #'rst-compile-alt-toolset
     "p" #'rst-compile-pdf-preview
     "s" #'rst-compile-slides-preview
     "x" #'rst-compile-pseudo-region)

    (:prefix ("l" . "bullets")
     "b" #'rst-bullet-list-region
     "t" #'rst-shift-region
     "c" #'rst-convert-bullets-to-enumeration
     "e" #'rst-enumerate-region
     "s" #'rst-straighten-bullets-region)

    (:prefix ("t" . "toc")
     "t" #'rst-toc
     "l" #'rst-toc-follow-link
     "u" #'rst-toc-update
     "i" #'rst-toc-insert)

    (:prefix ("s" . "section")
     "s" #'rst-straighten-sections
     "m" #'rst-mark-section
     "j" #'rst-forward-section
     "k" #'rst-backward-section))))
