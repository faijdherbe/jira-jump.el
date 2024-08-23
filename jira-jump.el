;;; jira-jump.el --- Quickly jump to a Jira issue or board.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jeroen Faijdherbe.

;; Author: J. Faijdherbe <jeroen@faijdherbe.net>
;; Version: 1.3
;; Created: 3 Feb 2023
;; Keywords: jira, browser, org-mode
;; URL: https://github.com/faijdherbe/jira-jump.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides a simple interactive function that allows you
;; to open any known project or issue in your browser.

;;; Code:

(defvar jira-jump--projects
  '()
  "Alist of Jira projects per host. This doc needs to be expanded.")

(defun jira-jump--tag-for-issue (issue)
  "Returns the TAG part of a Jira ISSUE.  Currently it splits the
given ISSUE on the - character, and returns the car of the
result.  We might want to change this to a regex match, grabbing
all alpha characters from the start of the string. "
  (upcase (car (split-string issue "-"))))

(defun jira-jump--get-project (tag)
  "Looks up the jira project in jira-jump--projects based on TAG.
Returns nil if nothing is found."
  (let ((data nil))
    (dolist (project jira-jump--projects)
      (if (member tag (alist-get 'projects (cdr project)))
          (setq data project)))
    data))

(defun jira-jump--make-link (issue)
  "Creates url to correct Jira instance base on TAG in ISSUE (the
part before the -).  Returns nil if no project can be found for
given issue."
  (let* ((tag (jira-jump--tag-for-issue issue))
         (project (jira-jump--get-project tag))
         (instance (alist-get 'instance (cdr project))))
    (cond (instance (concat instance "/browse/" issue))
          (t nil))))

(defun jira-jump--all-project-tags ()
  "Collects all project tags from all configured instances in
=jira-jump--projects=."
  (apply #'append (mapcar (lambda (project)
                            (alist-get 'projects project))
                          jira-jump--projects)))

(defun jira-jump--read-issue ()
  (completing-read "Issue: " (jira-jump--all-project-tags)))
