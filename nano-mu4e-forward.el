;;; nano-command.el --- N Λ N O quick command -*- lexical-binding: t -*-

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
;; This an example showing how to quickly forwarf an email using
;; nano-command and mu4e. 

;;; Code:
(require 'nano-command)

(defun mu4e-quick-forward ()
  "This function allows to forward a mail to some recipients (mu4e)."
  
  (interactive)
  (when (or (derived-mode-p 'mu4e-headers-mode)
            (derived-mode-p 'mu4e-view-mode))
    (let ((recipients (nano-command " FORWARD"
                                    '(lambda ()
                                       (unless (boundp 'mail-abbrevs)
                                         (build-mail-abbrevs))
                                       (setq local-abbrev-table mail-abbrevs)
                                       (abbrev-mode t)))))
      (when (and (stringp recipients) (not (string-empty-p recipients)))
        (mu4e-compose 'forward)
        (sit-for 0.10)
        (with-current-buffer (car (buffer-list))
          (when (derived-mode-p 'mu4e-compose-mode)
            (insert recipients)
            (message-send-and-exit)))))))

(bind-key "H-f" #'mu4e-quick-forward)
