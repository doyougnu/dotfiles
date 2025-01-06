;; Majority of these functions taken and renamed (to make sense for me) from
;; http://doc.norang.ca/org-mode.html
;;
(require 'org-habit)

;;;###autoload
(defun dyg/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

;;;###autoload
(defun dyg/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
  Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (dyg/find-project-task)
      (if (equal (point) task)
          nil
        t))))

;;;###autoload
(defun dyg/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

;;;###autoload
(defun dyg/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

;;;###autoload
(defun dyg/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

;;;###autoload
(defun dyg/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar dyg/hide-scheduled-and-waiting-next-tasks t)

;;;###autoload
(defun dyg/toggle-next-task-display ()
  (interactive)
  (setq dyg/hide-scheduled-and-waiting-next-tasks (not dyg/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if dyg/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

;;;###autoload
(defun dyg/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (if (dyg/is-project-p)
        (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
               (has-next ))
          (save-excursion
            (forward-line 1)
            (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
              (unless (member "WAITING" (org-get-tags))
                (setq has-next t))))
          (if has-next
              nil
            next-headline)) ; a stuck project, has subtasks but no next task
      nil))))

;;;###autoload
(defun dyg/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (dyg/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (dyg/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

;;;###autoload
(defun dyg/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (dyg/list-sublevels-for-projects-indented)
  (if (save-excursion (dyg/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((dyg/is-project-p)
            nil)
           ((and (dyg/is-project-subtree-p) (not (dyg/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

;;;###autoload
(defun dyg/skip-non-tasks ()
  "Show non-project tasks.
  Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((dyg/is-task-p)
        nil)
       (t
        next-headline)))))

;;;###autoload
(defun dyg/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((dyg/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun dyg/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and dyg/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags)))
        next-headline)
       ((dyg/is-project-p)
        next-headline)
     ((and (dyg/is-task-p) (not (dyg/is-project-subtree-p)))
      next-headline)
     (t
      nil)))))

;;;###autoload
(defun dyg/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
   When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
   When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((dyg/is-project-p)                                      next-headline)
       ((org-is-habit-p)                                        subtree-end)
       ((and (not limit-to-project) (dyg/is-project-subtree-p)) subtree-end)
       ((and limit-to-project
             (dyg/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
                                                                subtree-end)
       (t                                                       nil)))))

;;;###autoload
(defun dyg/skip-project-tasks ()
  "Show non-project tasks.
  Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((dyg/is-project-p)         subtree-end)
       ((org-is-habit-p)           subtree-end)
       ((dyg/is-project-subtree-p) subtree-end)
       (t                            nil)))))

;;;###autoload
(defun dyg/skip-non-project-tasks ()
  "Show project tasks.
  Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((dyg/is-project-p)                                   next-headline)
       ((org-is-habit-p)                                     subtree-end)
       ((and (dyg/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
                                                             subtree-end)
       ((not (dyg/is-project-subtree-p))                     subtree-end)
       (t                                                    nil)))))

;;;###autoload
(defun dyg/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((dyg/is-project-p) subtree-end)
       ((org-is-habit-p)   subtree-end)
       (t                  nil)))))

;;;###autoload
(defun dyg/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (dyg/is-subproject-p)
        nil
      next-headline)))

;; Remove empty LOGBOOK drawers on clock out
;;;###autoload
(defun dyg/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

;;;###autoload
(defun dyg/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))


;;;###autoload
(defun dyg/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

;;;###autoload
(defun dyg/org-open-default-todo-file ()
  "Open the default org notes file"
  (interactive)
  (if (file-exists-p org-default-todo-file)
      (find-file-other-window org-default-todo-file)
    (message "Org Default notes file not found. Set it with 'org-default-todo-file")
      ))

;;;###autoload
(defun dyg/org-open-default-roam-file ()
  "Open the default org notes file"
  (interactive)
  (if (file-exists-p org-roam-inbox)
      (find-file-other-window org-roam-inbox)
    (message "Org Default notes file not found. Set it with 'org-default-todo-file")
      ))

;;;###autoload
(defun dyg/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

;;;###autoload
(defun dyg/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))

;;;###autoload
(defun my/org-copy-refile-to-clipboard ()
  "Copy the most recently refiled entry to the system clipboard."
  (when (and (boundp 'org-refile-marker)
             org-refile-marker)
    (let* ((marker org-refile-marker)
           (buffer (marker-buffer marker))
           (pos (marker-position marker))
           (refiled-entry (with-current-buffer buffer
                            (save-excursion
                              (goto-char pos)
                              (org-back-to-heading t)
                              (let ((start (point)))
                                (org-end-of-subtree)
                                (buffer-substring-no-properties start (point)))))))
      (kill-new refiled-entry)
      (message "Refiled entry copied to clipboard: %s" refiled-entry))))


;;;;;;;;;;;;;;;;;;;;;;;;; Org Agenda Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; orgmode organization stuff
(setq-default org-default-todo-file "~/sync/org/refile.org"
              org-default-issue-file "~/sync/org/refile.org")

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
      '(("a" "Agenda"
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

  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images #'dyg/babel-ansi)
