;;; todoist.el --- Extension for interacting and managing todoist tasks -*- lexical-binding: t; -*-

;; Copyright (C) 2019, Adrien Brochard

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 1.0
;; Author: Adrien Brochard
;; Keywords: todoist task todo comm
;; URL: https://github.com/abrochard/emacs-todoist
;; License: GNU General Public License >= 3
;; Package-Requires: ((dash "2.15.0") (transient "0.1.0") (org "8.3.5") (emacs "25.3"))

;;; Commentary:

;; Emacs extension for interacting with the task tracking service todoist

;;; Setup:

;; Get your token (https://todoist.com/Users/viewPrefs?page=integrations)
;; and shove it as (setq todoist-token "XXXXXXXX")

;;; Usage:

;; M-x todoist to pull up tasks
;; C-x t for the task menu
;; C-x p for the project menu

;;; Code:

(require 'dash)
(require 'transient)
(require 'org)
(require 'json)
(require 'url)

(defvar todoist-token
  (getenv "TODOIST_TOKEN"))

(defconst todoist-url
  "https://beta.todoist.com/API/v8")

(defconst todoist-buffer-name
  "*todoist*")

(defvar todoist--cached-projects nil)

(defun todoist--query (method endpoint &optional data)
  "Main function to interact with Todoist api.

METHOD is http method string.
ENDPOINT is the endpoint string.
DATA is the request body."
  (let ((url (concat todoist-url endpoint))
        (url-request-method method)
        (url-request-extra-headers (append`(("Authorization" . ,(concat "Bearer " todoist-token)))
                                          (when data '(("Content-Type". "application/json")))))
        (url-request-data data))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      ;; (message (buffer-string))
      (if (equal (buffer-substring (point) (point-max)) "\n") ;; no body
          nil
        (json-read)))))

(defun todoist--task-id (task)
  "Get the task id.

TASK is the task object."
  (assoc-default 'id task))

(defun todoist--task-date (task)
  "Get the task due date.

TASK is the task object"
  (assoc-default 'date (assoc-default 'due task)))

(defun todoist--task-content (task)
  "Get the task content.

TASK is the task object."
  (assoc-default 'content task))

(defun todoist--task-project-id (task)
  "Get the task project id.

TASK is the task object"
  (assoc-default 'project_id task))

(defun todoist--project-id (project)
  "Get the project id.

PROJECT is the project object."
  (assoc-default 'id project))

(defun todoist--project-name (project)
  "Get the project name.

PROJECT is the project object."
  (assoc-default 'name project))

(defun todoist--filter-tasks (project tasks)
  "Get subset of tasks under a project.

PROJECT the project.
TASKS the list of tasks."
  (-filter (lambda (task)
             (equal (todoist--task-project-id task) (todoist--project-id project)))
           tasks))

(defun todoist--insert-heading (level str &optional todo)
  "Insert a org heading at a certain depth.

LEVEL is the level.
STR is the heading title.
TODO is optional to make this a todo heading."
  ;; (end-of-line)
  (end-of-buffer)
  (if todo
      (insert (format "\n%s TODO %s" (make-string level ?*) str))
    (insert (format "\n%s %s" (make-string level ?*) str))))

(defun todoist--insert-task (task level todo)
  "Insert the task as 'org-mode' bullet.

TASK is the task.
LEVEL is the ord heading level.
TODO is boolean to show TODO tag."
  (todoist--insert-heading level (todoist--task-content task) todo)
  (when (todoist--task-date task)
    (org-deadline nil (todoist--task-date task)))
  (org-set-property "TODOIST_ID" (format "%s" (todoist--task-id task)))
  (org-set-property "TODOIST_PROJECT_ID" (format "%s" (todoist--task-project-id task))))

(defun todoist--insert-project (project tasks)
  "Insert the current project and matching tasks as org buttet list.

PROJECT the project object.
TASKS the list of all tasks."
  (todoist--insert-heading 2 (todoist--project-name project))
  (mapcar (lambda (task) (todoist--insert-task task 3 nil))
          (todoist--filter-tasks project tasks)))

(defun todoist--inbox-id (projects)
  "Get the project id of the inbox.

PROJECTS the list of all projects."
  (todoist--project-id (-first (lambda (project) (equal (todoist--project-name project) "Inbox")) projects)))

(defun todoist--is-today (task)
  "Return non-nil if the tasks is scheduled for today or overdue.

TASK is the task."
  (and
   (todoist--task-date task)
   (time-less-p (date-to-time
                 (concat (todoist--task-date task) " 00:00:00"))
                (current-time))))

(defun todoist--insert-today (tasks)
  "Insert today's tasks.

TASKS is the list of tasks."
  (mapcar (lambda (task) (todoist--insert-task task 2 t)) (-filter 'todoist--is-today tasks)))

(defun todoist--get-projects (&optional cache)
  "Get the list of all projects.

CACHE to read from cache rather than query upstream."
  (if cache
      todoist--cached-projects
    (setq todoist--cached-projects
          (append (todoist--query "GET" "/projects") nil))))

(defun todoist--get-tasks ()
  "Get the list of all tasks."
  (append (todoist--query "GET" "/tasks") nil))

(defun todoist--fold-projects ()
  "Fold the project list."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\* Projects$")
    (org-cycle)))

(defun todoist--fold-today ()
  "Fold the today list to hide properties."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\* Today$")
    (org-cycle) (org-cycle)))

(defun todoist--under-cursor-task-id ()
  "Get the todoist task id of the task under the cursor."
  (save-excursion
    (org-back-to-heading)
    (org-element-property :TODOIST_ID (org-element-at-point))))

(defun todoist--parse-org-time-string (str)
  "Parse an org timestring into a simple YYYY-MM-DD string.

STR is an org time string."
  (unless (eq str nil)
    (string-match "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}+\\) .*>" str)
    (match-string 1 str)))


(defun todoist--select-project (&optional cache)
  "Take user input to select a project object from a list.

CACHE is optional param to get projects from cache."
  (let* ((projects (todoist--get-projects cache))
         (name (completing-read "Select project: " (mapcar 'todoist--project-name projects))))
    (-find (lambda (x) (equal (todoist--project-name x) name)) projects)))

;;; interactive
;;; project management
(defun todoist-new-project (name)
  "Create a new project.

NAME is the name of the project."
  (interactive "sProject name: ")
  (todoist--query "POST" "/projects" (json-encode `(("name" . ,name)))))

(defun todoist-update-project ()
  "Change the name of a project."
  (interactive)
  (todoist--query "POST" (format "/projects/%s" (todoist--project-id (todoist--select-project)))
                  (json-encode `(("name" . ,(read-string "New project name: "))))))

(defun todoist-delete-project ()
  "Delete a project."
  (interactive)
  (todoist--query "DELETE" (format "/projects/%s" (todoist--project-id (todoist--select-project)))))

;;; task managmement
(defun todoist-new-task (content due p)
  "Create a new task.

CONTENT is the content string.
DUE is the human friendly due string and can be empty.
P is a prefix argument to select a project."
  (interactive "sTask content: \nsDue: \nP")
  (todoist--query "POST" "/tasks"
                  (json-encode (append `(("content" . ,content) ("due_string" . ,due))
                                       (when p
                                         `(("project_id" . ,(todoist--project-id (todoist--select-project))))))))
  (todoist))

(defun todoist-update-task ()
  "Update the content and due date of the task under cursor."
  (interactive)
  (let ((task-id (todoist--under-cursor-task-id))
        (content (read-string "Task content: " (org-entry-get nil "ITEM")))
        (due (read-string "Task due: " (todoist--parse-org-time-string (org-entry-get nil "DEADLINE")))))
    (todoist--query "POST" (format "/tasks/%s" task-id) (json-encode `(("content" . ,content) ("due_string" . ,due))))
    (todoist)))

;; doesn't work??
;; (defun todoist-assign-task ()
;;   "Assign the task under the cursor to a specific project."
;;   (interactive)
;;   (let ((task-id (todoist--under-cursor-task-id))
;;         (project-id (todoist--project-id (todoist--select-project))))
;;     (todoist--query "POST" (format "/tasks/%s" task-id) (json-encode `(("project_id" . ,project-id))))
;;     (todoist)))

(defun todoist-delete-task ()
  "Delete the task under the cusror."
  (interactive)
  (todoist--query "DELETE" (format "/tasks/%s" (todoist--under-cursor-task-id)))
  (todoist))

(defun todoist-close-task ()
  "Close the task under the cursor, marking it as done or checking it."
  (interactive)
  (todoist--query "POST" (format "/tasks/%s/close" (todoist--under-cursor-task-id)))
  (todoist))

;; transient interface
(define-transient-command todoist-task-menu ()
  "Manage Todoist tasks."
  ["Actions"
   ("c" "Close task" todoist-close-task)
   ("n" "New task" todoist-new-task)
   ("u" "Update task" todoist-update-task)
   ("d" "Delete task" todoist-delete-task)])

(define-transient-command todoist-project-menu ()
  "Manage Todoist projects."
  ["Actions"
   ("n" "New project" todoist-new-project)
   ("u" "Update project" todoist-update-project)
   ("d" "Delete project" todoist-delete-project)])

(defvar todoist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x t") 'todoist-task-menu)
    (define-key map (kbd "C-x p") 'todoist-project-menu)
   map)
  "Keymap for `todoist-mode'.")

(define-derived-mode todoist-mode org-mode "Todoist"
  "Special mode for todoist buffers."
  (setq mode-name "Todoist")
  (setq major-mode 'todoist-mode)
  (use-local-map todoist-mode-map)
  (run-mode-hooks 'kubel-mode-hook))

;; main function
(defun todoist ()
  "Main function to summon the todoist dashboard as 'org-mode'."
  (interactive)
  (let ((projects (todoist--get-projects))
         (tasks (todoist--get-tasks)))
    (with-output-to-temp-buffer todoist-buffer-name
      (switch-to-buffer todoist-buffer-name)
      (todoist-mode)
      (insert "#+title: Todoist\n")
      (todoist--insert-heading 1 "Today")
      (todoist--insert-today tasks)
      (todoist--insert-heading 1 "Projects")
      (mapcar (lambda (project) (todoist--insert-project project tasks)) projects)
      (todoist--fold-projects)
      (todoist--fold-today))))

(provide 'todoist)
;;; todoist.el ends here
