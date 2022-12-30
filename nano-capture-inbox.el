;;; nano-capture-inbox.el --- N Λ N O quick command -*- lexical-binding: t -*-

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
;; This is an example on how to implement org-capture inbox using nano-command.
;; The inbox file is hard-coded but it is quite simple to change.

;;; Code:
;; (require 'nano-command)

(defconst nano-capture-inbox-file   "~/Documents/org/inbox.org")
(defconst nano-capture-inbox-target "")

(defun nano-capture-inbox-hook ()
  (goto-char (point-min))
  (org-mode))

(defun nano-capture-inbox-refile (file headline)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))

(defun nano-capture-inbox ()
  "This function allows to add an item in inbox (oorg)"

  (interactive)
  (let ((capture (nano-command " INBOX" #'nano-capture-inbox-hook)))
    (when capture
      (let ((current-buffer (current-buffer))
            (timestamp (format-time-string (car org-time-stamp-formats))))
        (with-temp-buffer
          (insert (format "* TODO %s\n" capture))
          (insert ":PROPERTIES:\n")
          (insert (format ":ENTERED: %s\n" (substring timestamp 1 -1)))
          (insert ":END:\n")
          (goto-char (point-min))
          (nano-capture-inbox-refile nano-capture-inbox-file
                                     nano-capture-inbox-target))
        (switch-to-buffer current-buffer)))))

(bind-key "H-t" #'nano-capture-inbox)
