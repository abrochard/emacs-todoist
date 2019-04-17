;;; package --- Summary -*- lexical-binding: t; -*-

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
;; Keywords: todoist task todo
;; URL: https://github.com/abrochard/emacs-todoist
;; License: GNU General Public License >= 3
;; Package-Requires: ((dash "2.15.0") (emacs "25.3"))

;;; Commentary:

;; Emacs extension for interacting with the task tracking service todoist

;;; Setup:

;; Get your token

;;; Usage:

;; M-x todoist

;;; Code:

(require 'dash)

(defvar todoist-token
  (getenv "TODOIST_TOKEN"))

(defconst todoist-url
  "https://beta.todoist.com/API/v8")

(defconst todoist-buffer-name
  "*todoist*")

(defun todoist--query (method endpoint &optional data)
  "Main function to interact with Todoist api.

METHOD is http method string.
ENDPOINT is the endpoint string.
DATA is the request body."
  (let ((url (concat todoist-url endpoint))
        (url-request-method method)
        (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " todoist-token)))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-read))))

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

(defun todoist--insert-project (project tasks)
  "Insert the current project and matching tasks as org buttet list.

PROJECT the project object.
TASKS the list of all tasks."
  (todoist--insert-heading 2 (todoist--project-name project))
  (mapcar (lambda (task) (todoist--insert-heading 3 (todoist--task-content task)))
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

(defun todoist--insert-task (task)
  "Insert the task as 'org-mode' bullet.

TASK is the task."
  (todoist--insert-heading 2 (todoist--task-content task) t)
  (org-deadline nil (todoist--task-date task)))

(defun todoist--insert-today (tasks)
  "Insert today's tasks.

TASKS is the list of tasks."
  (mapcar 'todoist--insert-task (-filter 'todoist--is-today tasks)))

(defun todoist--get-projects ()
  "Get the list of all projects."
  (append (todoist--query "GET" "/projects") nil))

(defun todoist--get-tasks ()
  "Get the list of all tasks."
  (append (todoist--query "GET" "/tasks") nil))

(defun todoist--fold-projects ()
  "Fold the project list."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\* Projects$")
    (org-cycle)))

(defun todoist ()
  "Main function to summon the todoist dashboard as 'org-mode'."
  (interactive)
  (let ((projects (todoist--get-projects))
         (tasks (todoist--get-tasks)))
    (with-output-to-temp-buffer todoist-buffer-name
      (switch-to-buffer todoist-buffer-name)
      (org-mode)
      (insert "#+title: Todoist\n")
      (todoist--insert-heading 1 "Today")
      (todoist--insert-today tasks)
      (todoist--insert-heading 1 "Projects")
      (mapcar (lambda (project) (todoist--insert-project project tasks)) projects)
      (todoist--fold-projects))))

(provide 'todoist)
;;; todoist.el ends here
