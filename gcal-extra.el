;;; gcal-extra.el --- Additional commands for org-gcal -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/gcal-extra
;; Version: 0.1.0
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "28.1") (org-gcal "0.4.2"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Additional commands for `org-gcal'

;;; Code:

(declare-function org-entry-get "org")

(defvar org-directory)

(require 'org-gcal)

(defun gcal-extra--setup-pass-var (var)
  "Set variable VAR with password-store entry or user input.

Argument VAR is a symbol representing the variable to set up with a password
entry."
  (require 'auth-source-pass nil t)
  (let ((value (or
                (and (boundp 'auth-source-pass-filename)
                     (fboundp 'auth-source-pass-parse-entry)
                     auth-source-pass-filename
                     (when-let* ((dir
                                  (and
                                   (file-directory-p auth-source-pass-filename)
                                   (file-name-as-directory
                                    (expand-file-name
                                     auth-source-pass-filename))))
                                 (entries (mapcar
                                           (lambda (file)
                                             (file-name-sans-extension
                                              (substring-no-properties (expand-file-name
                                                                        file)
                                                                       (length
                                                                        dir))))
                                           (directory-files-recursively
                                            auth-source-pass-filename
                                            "\\.gpg\\'"
                                            nil)))
                                 (found (completing-read
                                         (format "Pass entry for `%s': " var)
                                         entries
                                         nil t
                                         (when (seq-filter
                                                (apply-partially
                                                 #'string-match-p
                                                 (symbol-name
                                                  var))
                                                entries)
                                           (symbol-name var))))
                                 (parsed (auth-source-pass-parse-entry
                                          found))
                                 (field (completing-read "Field: " parsed
                                                         nil t)))
                       (cdr (or (assoc field parsed)
                                (assq (intern field) parsed)))))
                (read-string (format "Value for `%s': " var)))))
    (when value
      (if (and (get var 'custom-type)
               (yes-or-no-p (format "Save value for `%s'?" var)))
          (customize-save-variable var value "Saved by gcal-extra")
        (set-default var value)))))

(defun gcal-extra-setup (&optional force)
  "Configure Google Calendar integration for `org-gcal'.

Optional argument FORCE is a boolean indicating whether to force the setup
process.

Setup includes configuring variables `org-gcal-client-id',
`org-gcal-client-secret', `oauth2-auto-additional-providers-alist',
`org-gcal-fetch-file-alist'."
  (let ((providers-should-update)
        (vars-alist '((org-gcal-client-id . client_id)
                      (org-gcal-client-secret . client_secret))))
    (pcase-dolist (`(,sym . ,oauth-field) vars-alist)
      (when (boundp sym)
        (when (or force (not (symbol-value sym)))
          (gcal-extra--setup-pass-var sym))
        (let ((provider
               (cdr
                (assq 'org-gcal
                      oauth2-auto-additional-providers-alist)))
              (value (symbol-value sym)))
          (when (or (not provider)
                    (not (equal (assq oauth-field provider)
                                value)))
            (setq providers-should-update t)))))
    (when (and org-gcal-client-secret
               org-gcal-client-id
               providers-should-update)
      (let ((providers (assq-delete-all 'org-gcal
                                        oauth2-auto-additional-providers-alist)))
        (setq providers (push `(org-gcal
                                (authorize_url . "https://accounts.google.com/o/oauth2/auth")
                                (token_url . "https://oauth2.googleapis.com/token")
                                (scope . "https://www.googleapis.com/auth/calendar")
                                (client_id . ,org-gcal-client-id)
                                (client_secret . ,org-gcal-client-secret))
                              providers))
        (customize-save-variable 'oauth2-auto-additional-providers-alist
                                 providers
                                 "Saved by gcal-extra"))))
  (unless (bound-and-true-p org-gcal-fetch-file-alist)
    (let* ((email (read-string "Google email for calendar: " user-mail-address))
           (file
            (read-file-name "File to store calendar events: "
                            (file-name-as-directory org-directory)
                            nil
                            nil
                            "calendar.org")))
      (customize-save-variable 'org-gcal-fetch-file-alist
                               `((,email . ,file))
                               "Saved by gcal-extra"))))

;;;###autoload
(defun gcal-extra-fetch (&optional force)
  "Setup `org-gcal' and Fetch Google Calendar events into Org mode.

Optional argument FORCE is a boolean indicating whether to force the setup
process."
  (interactive "P")
  (gcal-extra-setup force)
  (org-gcal-fetch))

;;;###autoload
(defun gcal-extra-sync (&optional force)
  "Setup `org-gcal' and synchronize Org agenda with Google Calendar.

Optional argument FORCE is a boolean indicating whether to force the
synchronization process."
  (interactive "P")
  (gcal-extra-setup force)
  (org-gcal-sync))

(defun gcal-extra--make-toggle-description (description value &optional on-label
                                                        off-label left-separator
                                                        right-separator)
  "Create a toggle DESCRIPTION with alignment and optional labels.

Argument DESCRIPTION is a string that represents the description of the toggle.

Argument VALUE is a boolean indicating the current state of the toggle.

Optional argument ON-LABEL is a string used when VALUE is non-nil. It defaults
to \"+\".

Optional argument OFF-LABEL is a string used when VALUE is nil. It defaults to
\"-\".

Optional argument LEFT-SEPARATOR is a string placed before the ON-LABEL or
OFF-LABEL. It has no default value.

Optional argument RIGHT-SEPARATOR is a string placed after the ON-LABEL or
OFF-LABEL. It has no default value."
  (let* ((description (or description ""))
         (align (apply #'max (list (+ 5 (length description))
                                   30))))
    (concat
     (or description "")
     (propertize " " 'display (list 'space :align-to align))
     (or left-separator "")
     (if value
         (propertize
          (or on-label "+")
          'face
          'success)
       (propertize
        (or off-label "-")
        'face
        'transient-inactive-value))
     (or right-separator ""))))

;;;###autoload (autoload 'gcal-extra-menu "gcal-extra" nil t)
(transient-define-prefix gcal-extra-menu ()
  "Select and invoke an EasyPG command from a list of available commands."
  :transient-suffix     #'transient--do-call
  :transient-non-suffix #'transient--do-stay
  :refresh-suffixes t
  [("D" "Delete entry at point to current calendar" org-gcal-delete-at-point
    :inapt-if-not gcal-extra--on-gcal-entry
    :transient nil)
   ("p" "Post entry at point to current calendar" org-gcal-post-at-point
    :inapt-if-not gcal-extra--on-gcal-entry
    :transient nil)]
  [("s" "Sync all events" org-gcal-sync
    :transient nil)
   ("y" "Sync buffer with Calendar (fetch and post)" org-gcal-sync-buffer
    :transient nil)
   ("f" "Fetch event data from google calendar" org-gcal-fetch
    :transient nil)
   ("e" "Fetch without posting changes" org-gcal-fetch-buffer
    :transient nil)]
  [("d" org-gcal-toggle-debug
    :description
    (lambda ()
      (gcal-extra--make-toggle-description "Debug"
                                           (bound-and-true-p org-gcal-debug)
                                           "+"
                                           "" "[" "]"))
    :transient t)
   ("u" org-gcal--sync-unlock
    :description
    (lambda ()
      (gcal-extra--make-toggle-description "Deactivate sync lock "
                                           (bound-and-true-p
                                            org-gcal--sync-lock)
                                           "+"
                                           "" "[" "]"))
    :inapt-if-nil org-gcal--sync-lock)
   ("O" "Setup OAuth2 authentication after setting client id and secret"
    org-gcal-reload-client-id-secret
    :transient nil)
   ("C" "Clear all Calendar api sync tokens" org-gcal-sync-tokens-clear
    :transient nil)])

(defun gcal-extra--in-gcal-buffer ()
  "Check if current buffer is associated with a Google Calendar file."
  (when-let ((file buffer-file-name))
    (seq-find (pcase-lambda (`(,_k . ,gcal-file))
                (and gcal-file
                     (string=
                      file
                      (expand-file-name
                       gcal-file))))
              org-gcal-fetch-file-alist)))

(defun gcal-extra--on-gcal-entry ()
  "Check for Google Calendar managed property at point."
  (require 'org-gcal nil t)
  (when (bound-and-true-p org-gcal-managed-property)
    (unless (bound-and-true-p org-gcal--sync-lock)
      (org-entry-get (point) org-gcal-managed-property))))

(defun gcal-extra--invoke-menu ()
  "Display Google Calendar menu for valid entries."
  (when (and (gcal-extra--in-gcal-buffer)
             (gcal-extra--on-gcal-entry))
    (transient-setup #'gcal-extra-menu)))

;;;###autoload
(define-minor-mode gcal-extra-mode
  "Toggle Google Calendar menu in Org mode.

Toggle integration of Google Calendar with Org mode, adding a custom action to
`\\[org-ctrl-c-ctrl-c]' command on calendar entries."
  :lighter " gcal-extra-menu"
  :global nil
  (remove-hook 'org-ctrl-c-ctrl-c-final-hook #'gcal-extra--invoke-menu
               'local)
  (when gcal-extra-mode
    (add-hook 'org-ctrl-c-ctrl-c-hook #'gcal-extra--invoke-menu nil
              'local)))

(provide 'gcal-extra)
;;; gcal-extra.el ends here
