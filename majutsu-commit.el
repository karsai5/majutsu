;;; majutsu-commit.el --- Commit command for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library implements jj commit and describe frontends using with-editor
;; and log context defaults.  The commit transient provides options for
;; selecting co-authors and running the git pre-commit hook.

;;; Code:

(require 'majutsu)
(require 'majutsu-jj)
(require 'majutsu-process)
(require 'majutsu-jjdescription)
(require 'subr-x)
(require 'seq)

;;; Co-author helpers

(defvar majutsu-commit--coauthors nil
  "List of selected co-authors for the next commit.
Each entry is a string of the form \"Name <email>\".")

(defvar majutsu-commit--run-pre-commit nil
  "When non-nil, run the git pre-commit hook before committing.")

(defun majutsu-commit--repo-authors ()
  "Return a list of unique authors from the repo, most recent first.
Each entry is a string of the form \"Name <email>\"."
  (let* ((template "if(author.name(), author.name() ++ \" <\" ++ author.email() ++ \">\\n\", \"\")")
         (output (majutsu-jj-string
                  "log" "--no-graph" "--ignore-working-copy"
                  "-T" template
                  "-r" "all()"))
         (lines (split-string (or output "") "\n" t "[ \t]+"))
         (seen (make-hash-table :test #'equal))
         result)
    ;; Preserve order (most recent first) while deduplicating.
    (dolist (line lines)
      (unless (gethash line seen)
        (puthash line t seen)
        (push line result)))
    (nreverse result)))

(defun majutsu-commit--read-coauthors (&rest _)
  "Read co-authors interactively from the repo's author list."
  (let* ((authors (majutsu-commit--repo-authors))
         (selected (completing-read-multiple
                    "Co-author(s) (comma-separated): " authors nil nil)))
    (setq selected (seq-filter (lambda (s) (not (string-empty-p s))) selected))
    selected))

;;; Co-author injection into the editor buffer

(defun majutsu-commit--coauthor-trailer-lines (coauthors)
  "Return a string of Co-authored-by trailer lines for COAUTHORS."
  (mapconcat (lambda (author)
               (format "Co-authored-by: %s" author))
             coauthors "\n"))

(defun majutsu-commit--inject-coauthors-visitor (coauthors)
  "Return a with-editor visit function that appends COAUTHORS trailers.
Trailers are inserted above the JJ: comment block."
  (let ((done nil))
    (lambda ()
      (when (and (not done)
                 (majutsu--with-editor--target-buffer-p))
        (setq done t)
        (goto-char (point-min))
        (let ((insert-pos
               (if (re-search-forward "^JJ:" nil t)
                   (line-beginning-position)
                 (point-max))))
          (goto-char insert-pos)
          ;; Ensure a blank separator line before the trailers.
          (unless (save-excursion
                    (forward-line -1)
                    (looking-at-p "^[ \t]*$"))
            (insert "\n"))
          (insert (majutsu-commit--coauthor-trailer-lines coauthors))
          (insert "\n\n"))
        ;; Move point back to the beginning for the user to type the message.
        (goto-char (point-min))
        t))))

;;; Pre-commit hook

(defun majutsu-commit--git-root ()
  "Return the underlying git directory for the current jj repo, or nil."
  (let ((output (string-trim (or (majutsu-jj-string "git" "root") ""))))
    (when (and (not (string-empty-p output))
               (file-directory-p output))
      output)))

(defun majutsu-commit--pre-commit-hook-path ()
  "Return the path to the git pre-commit hook, or nil if it doesn't exist."
  (when-let* ((git-root (majutsu-commit--git-root))
              (hook (expand-file-name "hooks/pre-commit" git-root)))
    (when (file-executable-p hook)
      hook)))

(defun majutsu-commit--run-pre-commit-hook (callback)
  "Run the git pre-commit hook asynchronously.
Call CALLBACK with no arguments on success.  On failure, display
the output in the process buffer and message the user.
The process buffer is shown immediately so the user can follow progress."
  (let ((hook (majutsu-commit--pre-commit-hook-path)))
    (if (not hook)
        ;; No hook found — proceed immediately.
        (funcall callback)
      (message "Running pre-commit hook...")
      (let ((process (majutsu-start-process hook)))
        ;; Show the process buffer so the user can see progress.
        (when-let* ((buf (process-buffer process)))
          (majutsu-display-buffer buf 'process))
        (process-put process 'inhibit-refresh t)
        (process-put process 'finish-callback
                     (lambda (_process exit-code)
                       (if (zerop exit-code)
                           (progn
                             (message "Pre-commit hook passed")
                             (funcall callback))
                         (message "Pre-commit hook FAILED (exit %d) — commit aborted"
                                  exit-code))))))))

;;; Transient infixes

(defun majutsu-commit--coauthors-description ()
  "Format the co-author infix description with current selections."
  (if majutsu-commit--coauthors
      (format "Co-author(s) (%s)"
              (propertize (string-join majutsu-commit--coauthors ", ")
                          'face 'transient-value))
    "Co-author(s)"))

(transient-define-infix majutsu-commit-infix-coauthors ()
  :description 'majutsu-commit--coauthors-description
  :class 'transient-lisp-variable
  :variable 'majutsu-commit--coauthors
  :key "-a"
  :reader #'majutsu-commit--read-coauthors
  :format " %k %d")

(defun majutsu-commit--pre-commit-description ()
  "Format the pre-commit infix description with current state."
  (if majutsu-commit--run-pre-commit
      (format "Run pre-commit hook (%s)"
              (propertize "on" 'face 'transient-value))
    "Run pre-commit hook"))

(defun majutsu-commit--read-pre-commit (&rest _)
  "Toggle the pre-commit hook flag."
  (not majutsu-commit--run-pre-commit))

(transient-define-infix majutsu-commit-infix-pre-commit ()
  :description 'majutsu-commit--pre-commit-description
  :class 'transient-lisp-variable
  :variable 'majutsu-commit--run-pre-commit
  :key "-p"
  :reader #'majutsu-commit--read-pre-commit
  :format " %k %d")

;;; Execute

(defun majutsu-commit--do-commit (coauthors)
  "Run jj commit with the editor, injecting COAUTHORS if non-nil."
  (when coauthors
    (majutsu--with-editor--queue-visit
     (majutsu-commit--inject-coauthors-visitor coauthors)))
  (majutsu-run-jj-with-editor '("commit")))

;;;###autoload
(defun majutsu-commit-execute ()
  "Execute jj commit using the current transient selections."
  (interactive)
  (let ((coauthors majutsu-commit--coauthors)
        (run-hook majutsu-commit--run-pre-commit))
    ;; Clear state for the next invocation.
    (setq majutsu-commit--coauthors nil)
    (setq majutsu-commit--run-pre-commit nil)
    (if run-hook
        (majutsu-commit--run-pre-commit-hook
         (lambda () (majutsu-commit--do-commit coauthors)))
      (majutsu-commit--do-commit coauthors))))

;;; Commit Transient

(defun majutsu-commit--description ()
  "Compose the transient description for jj commit."
  (let (parts)
    (when majutsu-commit--coauthors
      (push (format "Co-authors: %s"
                     (string-join majutsu-commit--coauthors ", "))
            parts))
    (when majutsu-commit--run-pre-commit
      (push "Pre-commit: on" parts))
    (if parts
        (concat "JJ Commit | " (string-join (nreverse parts) " | "))
      "JJ Commit")))

;;;###autoload (autoload 'majutsu-commit-transient "majutsu-commit" nil t)
(transient-define-prefix majutsu-commit-transient ()
  "Transient for jj commit with co-author and pre-commit options."
  :man-page "jj-commit"
  :transient-non-suffix t
  [:description majutsu-commit--description
   :class transient-columns
   ["Options"
    (majutsu-commit-infix-coauthors)
    (majutsu-commit-infix-pre-commit)]
   ["Actions"
    ("c" "Commit" majutsu-commit-execute)
    ("RET" "Commit" majutsu-commit-execute)
    ("q" "Quit" transient-quit-one)]])

;;;###autoload
(defun majutsu-commit ()
  "Create a commit using the commit transient menu."
  (interactive)
  (majutsu-commit-transient))

;;; majutsu-describe

;;;###autoload
(defun majutsu-describe (&optional arg)
  "Update the description for the commit at point.
With prefix ARG, add --ignore-immutable."
  (interactive "P")
  (let* ((revset (or (magit-section-value-if 'jj-commit) "@"))
         (args (append (list "describe" "-r" revset)
                       (when arg '("--ignore-immutable")))))
    (majutsu-run-jj-with-editor args)))

;;; _
(provide 'majutsu-commit)
;;; majutsu-commit.el ends here
