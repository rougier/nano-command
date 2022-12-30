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
;; This an example showing how to start an asynchronous shell command.

;;; Code:
(require 'nano-command)

(bind-key "H-&" #'(lambda ()
                    (interactive)
                    (let ((command (nano-command " SHELL")))
                      (when command
                        (async-shell-command command)))))
