;;; nano-capture-meeting.el --- N Λ N O quick command -*- lexical-binding: t -*-

;; Copyright (C) 2022 Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-command
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: mode-line

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is an example on how to qucly capture a new meeting (org) using
;; nano-command. The meeting refile target is hard-coded but it is quite simple
;; to change (make sure to modify the "**" depending on your target level).

;;; Code:
;; (require 'nano-command)

(defconst nano-capture-agenda-file   "~/Documents/org/agenda.org")
(defconst nano-capture-agenda-target "Future")

(defun nano-capture-agenda-hook ()
  (insert (format-time-string (concat " "
                               (cdr org-time-stamp-formats))))
  (goto-char (point-min))
  (org-mode))
  
(defun nano-capture-agenda-refile (file headline)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))

(defun nano-capture-agenda ()
  "This function allows to register a meeting (org)"

  (interactive)
  (let ((capture (nano-command " MEETING" #'nano-capture-agenda-hook)))
    (when capture
      (let ((current-buffer (current-buffer)))
        (with-temp-buffer
          (insert (format "** %s\n" capture))
          (goto-char (point-min))
          (nano-capture-agenda-refile nano-capture-agenda-file
                                      nano-capture-agenda-target))
        (switch-to-buffer current-buffer)))))

(bind-key "H-m" #'nano-capture-agenda)

