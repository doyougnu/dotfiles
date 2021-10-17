;;; dyg/org-funcs.el -*- lexical-binding: t; -*-

;;; org-funcs.el --- My personal Org Config Layer for doom
;;
;; Author: Jeffrey Young <youngjef@oregonstate.edu>
;; URL: https://github.com/doyougnu/dyg-org
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Majority of these functions taken and renamed (to make sense for me) from
;; http://doc.norang.ca/org-mode.html
;;
(require 'org-habit)

;;;###autoload
(defun dyg-org/ping! ()
  (interactive)
  (message "pong!"))

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
