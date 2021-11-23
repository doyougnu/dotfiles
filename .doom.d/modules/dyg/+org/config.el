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

;;;;;;;;;;;;;;;;;;;;;;;;; Org General Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; this is the stuff that would normally be wrapped in a :variables. I like
  ;; keeping it grouped

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

  ;; don't set bookmarks on a capture
  (setf org-capture-bookmark             nil)

  ;; don't fontify the refile created bookmarks
  (setf bookmark-fontify                 nil)

                                        ; Exclude DONE state tasks from refile targets
  (setf org-refile-target-verify-function 'dyg/verify-refile-target)

  ;; never split headlines
  (setf org-M-RET-may-split-line           nil)
  (setf org-insert-heading-respect-content t)

  ;; use firefox
  (setf browse-url-browser-function 'browse-url-firefox)

;;;;;;;;;;;;;;;;;;;;;;;;; Org Agenda Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; orgmode organization stuff
  (setq-default org-default-todo-file "~/sync/org/refile.org")

  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))


  (setq org-agenda-files '("~/sync/org/research.org"
                           "~/sync/org/personal.org"
                           "~/sync/org/long-term.org"
                           "~/sync/org/job/"
                           "~/sync/org/projects.org"
                           "~/sync/org/refile.org"))





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
        '(("n" "Agenda"
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
          ("r" "respond" entry (file org-default-todo-file)
           "* NEXT Respond to %^{Prompt}%:from on %:subject\nSCHEDULED: %t\n%U\n" :clock-resume t :immediate-finish t :empty-lines 1)
          ("n" "note" entry (file org-default-todo-file)
           "* %? :NOTE:\n - Note taken on %U \\\\ \n" :clock-resume t :empty-lines 1)
          ("i" "idea" entry (file org-default-todo-file)
           "* %? :IDEA:\n - Idea taken on %U \\\\ \n" :clock-resume t :empty-lines 1)
          ("m" "meeting" entry (file org-default-todo-file)
           "* MEETING with %? :MEETING:\n%U" :clock-resume t :empty-lines 1)
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

;;;;;;;;;;;;;;;;;;;;;;;;; Custom key binds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (map!
   (:map org-mode-map
    :localleader
    (:prefix ("s" . "tree/subtree")
     "i" #'org-insert-subheading)

    (:prefix ("x" . "text")
     "c" #'dyg/org-insert-code
     "s" #'dyg/org-insert-strikethrough
     "i" #'dyg/org-insert-italics
     "u" #'dyg/org-insert-underline
     "b" #'dyg/org-insert-bold)

    (:prefix ("i" . "insert")
     "i" #'org-insert-item
     "b" #'org-insert-structure-template
     "c" #'dyg/org-insert-comment
     "l" #'org-insert-link
     "n" #'org-add-note))

   (:map org-capture-mode-map
    :localleader
    "," #'org-capture-finalize
    "k" #'org-capture-kill
    "r" #'org-capture-refile)

   :after flyspell
   :map org-mode-map
   :ni "C-;" #'fill-paragraph))

(map!
   :after evil-org-agenda
   :map evil-org-agenda-mode-map
   :m "S" #'org-save-all-org-buffers)

(map! :after evil-org
      :map evil-org-mode-map
      :ni "C-<return>" #'evil-org-org-insert-heading-respect-content-below
      :ni "S-<return>" #'org-meta-return)

(map! :after evil-org-agenda
      :map evil-org-agenda-mode-map
      :m "S" #'org-save-all-org-buffers)
