;;; +org/config.el -*- lexical-binding: t; -*-

;;; +org.el --- custom org module for doom
;;
;; Author: Jeffrey Young (doyougnu) <youngjef@oregonstate.edu>
;; URL: https://github.com/doyougnu/dyg-org
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(after! org

  ;; export github flavored markdown
  (require 'ox-gfm)
  (require 'org-pomodoro)

  ;;;;;;;;;;;;;;;;;;;;;;;;; Org General Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; this is the stuff that would normally be wrapped in a :variables. I like
  ;; keeping it grouped

  (setq org-pomodoro-play-sounds       nil)
  (setq org-pomodoro-long-break-frequency 3)
  (setq org-directory                  "~/sync/org")
  (setq evil-org-special-o/O           '(table-row item))
  (setq org-want-todo-bindings         t)
  (setq org-enforce-todo-dependencies  t)
  (setq org-clock-persist              'history)
  (setq org-clock-idle-time            15)
  (setq org-cycle-separator-lines      1)
  (setq org-enable-github-support      t)
  (setq org-src-tab-acts-natively      t)
  (setq org-projectile-file            "project.org")
  (setq org-enable-org-journal-support t)
  (setq org-journal-enable-cache       t)
  (setq org-journal-file-type          'weekly)
  (setq org-journal-dir                "~/sync/org/journal")
  (setq org-journal-file-format        "%Y/%m/%d.org")
  (setq org-columns-default-format     "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-carryover-items "TODO=\"NEXT\"|TODO=\"TODO\"|TODO=\"HOLD\"|TODO=\"INPROG\"")
  (setq org-confirm-babel-evaluate       nil)
  (setq org-edit-src-content-indentation 2)
  (setq org-use-fast-todo-selection      t)
  (setq org-refile-use-outline-path      t)
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . t)))

  ;; don't set bookmarks on a capture
  (setf org-bookmark-names-plist             nil)

  ;; request a note everytime we clock out on clocked in item
  (setf org-log-note-clock-out           t)

  ;; pomodoro
  (setf org-pomodoro-format
        (concat (format "%d|Pomodoro-"
                        (or org-pomodoro-count 0))
                "%s"))
  (add-hook! 'org-pomodoro-long-break-finished-hook #'org-add-note)
  (add-hook! 'org-after-refile-insert-hook 'dyg/org-copy-refile-to-clipboard)

  ;; always use listings for org latex export of code
  (setf org-latex-src-block-backend               'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")
                                   ("fontsize" "\\tiny"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; set latex to always use -shell-escape
  (setf LaTeX-command "latex -shell-escape")

  (setq org-latex-compiler "lualatex")
  (setq org-preview-latex-default-process 'dvisvgm)

  ;; don't fontify the refile created bookmarks
  (setf bookmark-fontify                 nil)

                                        ; Exclude DONE state tasks from refile targets
  (setf org-refile-target-verify-function 'dyg/verify-refile-target)

  ;; never split headlines
  (setf org-M-RET-may-split-line           nil)
  (setf org-insert-heading-respect-content t
        org-id-link-to-org-use-id          'create-if-interactive)

  ;; pretty mode for symbols
  (add-hook! 'org-mode-hook #'+org-pretty-mode #'(lambda ()
                                                   (add-hook 'evil-insert-state-entry-hook #'(lambda ()
                                                                                               (setq-local org-hide-emphasis-markers nil)))
                                                   (add-hook 'evil-insert-state-exit-hook #'(lambda ()
                                                                                              (setq-local org-hide-emphasis-markers t)))))

  (add-hook! 'org-mode-hook #'auto-fill-mode)
  (add-hook! 'before-save-hook :append #'dyg/org-fix-blank-lines)

  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; use firefox
  (setf browse-url-browser-function 'browse-url-firefox)

;;;;;;;;;;;;;;;;;;;;;;;;; Org Roam Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org roam config

  (after! org-roam

    (setq org-roam-directory (file-truename "~/sync/roam")
          org-roam-inbox     (concat org-roam-directory "/" "refile.org"))

    (setq org-roam-capture-templates
          '(("m" "main" plain "%?"
             :if-new (file+head "main/${slug}.org"
                                "#+title: ${title}\n")
             :immediate-finish t
             :unnarrowed t)
            ("l" "literature" plain "%?"
             :if-new
             (file+head "literature/${title}.org" "#+title: ${title}\n")
             :immediate-finish t
             :unnarrowed t)
            ("c" "concepts" plain "%?"
             :if-new
             (file+head "concepts/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
             :immediate-finish t
             :unnarrowed t)))

    (cl-defmethod org-roam-node-type ((node org-roam-node))
      "Return the TYPE of NODE."
      (condition-case nil
          (file-name-nondirectory
           (directory-file-name
            (file-name-directory
             (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (error "")))

    (setq org-roam-node-display-template
          (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

    (defun roam|tag-new-node-as-draft ()
      (org-roam-tag-add '("draft")))

    (add-hook! 'org-roam-capture-new-node-hook #'roam|tag-new-node-as-draft))

;;;;;;;;;;;;;;;;;;;;;;;;; Org Agenda Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; orgmode organization stuff
  (setq-default org-default-todo-file "~/sync/org/refile.org"
                org-default-issue-file "~/sync/org/job/projects/js-backend.org")

  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))


  (setq org-agenda-files (directory-files-recursively "~/sync/org/" "\\.org$"))

  ;; set the default view to a week starting on Monday
  (setq org-agenda-span 3
        org-agenda-start-day nil)

  (setq org-tag-alist '(("research"     . ?r)
                        ("job"          . ?j)
                        ("dnd"          . ?d)
                        ("chores"       . ?c)
                        ("dev-setup"    . ?s)
                        ("longterm"     . ?l)))


  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

  (setq org-agenda-compact-blocks t)

  ;; Custom agenda command definitions
  (setq org-agenda-custom-commands
        '((" " "Agenda"
           ((agenda "" nil)
            (tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to Refile")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "-CANCELLED/!"
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-skip-function     'dyg/skip-non-stuck-projects)
                        (org-agenda-sorting-strategy  '(category-keep))))
            (tags-todo "-HOLD-CANCELLED/!"
                       ((org-agenda-overriding-header "Projects")
                        (org-agenda-skip-function      'dyg/skip-non-projects)
                        (org-tags-match-list-sublevels 'indented)
                        (org-agenda-sorting-strategy   '(category-keep))))
            (tags-todo "-CANCELLED/!NEXT"
                       ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                              (if dyg/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'dyg/skip-projects-and-habits-and-single-tasks)
                        (org-tags-match-list-sublevels t)
                        (org-agenda-todo-ignore-scheduled dyg/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines dyg/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date dyg/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy      '(todo-state-down effort-up category-keep))))
            (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                       ((org-agenda-overriding-header (concat "Project Subtasks"
                                                              (if dyg/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'dyg/skip-non-project-tasks)
                        (org-agenda-todo-ignore-scheduled dyg/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines dyg/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date dyg/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy      '(category-keep))))
            (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                       ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                              (if dyg/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'dyg/skip-project-tasks)
                        (org-agenda-todo-ignore-scheduled dyg/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines dyg/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date dyg/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy      '(category-keep))))
            (tags-todo "-CANCELLED+WAITING|HOLD/!"
                       ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                              (if dyg/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function      'dyg/skip-non-tasks)
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-todo-ignore-scheduled dyg/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines dyg/hide-scheduled-and-waiting-next-tasks)))
            (tags "-REFILE/"
                  ((org-agenda-overriding-header "Tasks to Archive")
                   (org-agenda-skip-function 'dyg/skip-non-archivable-tasks)
                   (org-tags-match-list-sublevels nil))))
           nil)))

;;;;;;;;;;;;;;;;;;;;;;;;; Org Capture Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq org-capture-templates
        '(("t" "todo" entry (file org-default-todo-file)
           "* TODO %?\n - Todo made on %U \\\\ \n" :clock-resume t :empty-lines 1)
          ("o" "oneoff" entry (file org-default-todo-file)
           "* NEXT %?\n - OneOff made on %U \\\\ \n" :clock-resume t :empty-lines 1)
          ("j" "jira ticket" entry (file+headline org-default-issue-file "NEXT JS Backend")
           "* TODO LT-%^{Prompt}\n - Issue made on %U \\\\ \n" :clock-resume t :immediate-finish t :empty-lines 1)
          ("n" "note" item (function +default/org-notes-headlines)
           "Note taken on %U \\\\ \n%?"
           :clock-resume t :empty-lines 1)
          ("i" "idea" entry (file org-default-todo-file)
           "* %? :IDEA:\n - Idea taken on %U \\\\ \n" :clock-resume t :empty-lines 1)
          ("m" "meeting" entry (file org-default-todo-file)
           "* MEETING with %? :MEETING:\n%U" :clock-resume t :empty-lines 1)
          ("s" "Slipbox" entry  (file org-roam-inbox)
           "* %?\n")
          ))


  (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 9))))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

;;;;;;;;;;;;;;;;;;;;;;;;; Org Archiving Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq-default org-archive-default-directory "~/sync/org/.archive/")
  (setq org-archive-location (concat
                              org-archive-default-directory
                              "%s_archive::* Archived Tasks"))

;;;;;;;;;;;;;;;;;;;;;;;;; Org Publish Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq org-publish-project-alist
        '(("org-notes"
           :base-directory "~/Programming/blog/orgblog"
           :base-extension "org"
           :publishing-directory "~/Programming/blog/public_html/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4           ; Just the default for this project.
           :auto-preamble t)

          ("org-static"
           :base-directory "~/Programming/blog/orgblog"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/Programming/blog/public_html/"
           :recursive t
           :publishing-function org-publish-attachment)

          ("orgblog" :components ("org-notes" "org-static"))))

;;;;;;;;;;;;;;;;;;;;;;;;; Org Babel        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; template special for sicp
  (add-to-list 'org-structure-template-alist '("si" . "src scheme"))

  (add-hook! 'org-babel-after-execute-hook #'org-redisplay-inline-images #'dyg/babel-ansi)

;;;;;;;;;;;;;;;;;;;;;;;;; Custom key binds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (map!
   (:map org-mode-map
    :localleader
    "." #'org-insert-structure-template
    "," #'org-ctrl-c-ctrl-c
    "L" #'org-latex-preview
    "S" #'org-save-all-org-buffers
    "SPC" #'org-add-note

    (:prefix ("s" . "tree/subtree")
             "i" #'org-insert-subheading)

    (:prefix ("-" . "babel")
             "-" #'org-babel-execute-subtree
             "b" #'org-babel-execute-buffer
             "n" #'org-babel-next-src-block
             "e" #'org-babel-execute-src-block
             "p" #'org-babel-previous-src-block
             "s" #'org-babel-pop-to-session
             "S" #'org-babel-switch-to-session-with-code))

   (:map org-capture-mode-map
    :after org
    :localleader
    "c" #'org-capture-finalize
    "k" #'org-capture-kill
    "r" #'org-capture-refile)

   :after flyspell
   :map org-mode-map
   :ni "C-;" #'fill-paragraph)
  )

(map! :after evil-org
      :map evil-org-mode-map
      :ni "C-<return>" #'evil-org-org-insert-heading-respect-content-below
      :ni "S-<return>" #'org-meta-return)

(map! :after evil-org-agenda
      :map evil-org-agenda-mode-map
      :m "S" #'org-save-all-org-buffers)
