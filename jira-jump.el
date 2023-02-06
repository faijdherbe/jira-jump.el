;;; jira-jump.el --- Quickly jump to a Jira issue or board.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jeroen Faijdherbe.

;; Author: J. Faijdherbe <jeroen@faijdherbe.net>
;; Version: 1.1
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

(defun jira-jump (arg)
  "Open jira issue in browser.  A single prefix command will send
the link to the kill ring and a double prefix argument will
insert an org-mode link at point."
  (interactive "P")
  (let* ((issue (jira-jump--read-issue))
         (link (jira-jump--make-link issue)))
    (cond ((= 4 (prefix-numeric-value arg))
           (kill-new link)
           (message (format "Stored Jira link to issue %s (%s) in kill-ring."
                            issue
                            link)))
          ((= 16 (prefix-numeric-value arg))
           (insert (format "[[%s][%s]]"
                           link
                           issue)))
          (t
           (message (format "Opening issue %s in browser..." issue))
           (browse-url-default-browser link)))))

(add-to-list 'org-link-abbrev-alist
             '("jira" . "%(jira-jump--make-link)"))

(provide 'jira-jump)
;;; jira-jump.el ends here
