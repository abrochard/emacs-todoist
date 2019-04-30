# emacs-todoist
Emacs interface to todoist

## Installation
1. Load the `todoist.el` file
2. Get your Todoist personal API token [here](https://todoist.com/Users/viewPrefs?page=integrations)
3. Load it up via
   a. Elisp
       ```
       (setq todoist-token "XXXXXXXXXXXXXXXXXXXX")
       ```
   b. Environment variable as `TODOIST_TOKEN`
4. Call `M-x todoist` to pull up your tasks.

## TODO
 - [X] mark task as done
 - [X] update task name
 - [X] reschedule task
 - [X] delete task
 - [ ] create task under a project
 - [ ] own mode?
 - [ ] project CRUD operations

## Disclaimer
This extension is not created by, affiliated with, or supported by Doist.
