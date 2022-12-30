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

;;; Code:

(defgroup nano nil
  "N Λ N O"
  :group 'convenience)

(defgroup nano-command nil
  "N Λ N O Command"
  :group 'nano)

(defface nano-command-face
  `((t :foreground ,(face-foreground 'default)
       :background ,(face-background 'highlight nil t)
       :height ,(face-attribute 'default :height)))
  "Face for nano command"
  :group 'nano-command)

(defface nano-command-prompt-face
  `((t :foreground ,(face-background 'default)
       :background ,(face-foreground 'default)
       :weight ,(face-attribute 'bold :weight nil 'default)))
  "Face for prompt"
  :group 'nano-command)

(defface nano-command-region-face
  `((t :foreground ,(face-foreground 'region nil t)
       :background ,(face-background 'region nil t)))
  "Face for active region"
    :group 'nano-command)

(defface nano-command-cursor-face
  `((t :foreground ,(face-background 'default)
       :background ,(face-foreground 'default)))
  "Face for cursor"
  :group 'nano-command)

(defun nano-command--update (prompt text)
  "Update the mode or header line by dsplaying PROMPT and TEXT.
Note that TEXT is propertized such as to show the cursor
position. This method can be safely advised if user needs to display the command differently."
  
  (setq-local header-line-format (concat prompt text)))


(defun nano-command--process (current-buffer command-buffer prompt)
  "Process current command and prompt and update mode or header line"
  
  (with-current-buffer command-buffer
    (let* ((text (concat (buffer-substring (point-min) (point-max))
                         " "
                         (propertize " " 'display `(space :align-to (- right 2)))
                         (propertize " " 'face 'nano-faded)))
           (point (point))
           (end-of-line (= (point) (point-max)))
           (region-beg (if (use-region-p) (- (region-beginning) 1)))
           (region-end (if (use-region-p) (region-end))))
      (add-face-text-property (- point 1) point 'nano-command-cursor-face t text)
      (when (and region-beg region-end)
        (add-face-text-property region-beg region-end 'nano-command-region-face t text))

      (let ((space 1))
        (if (> point (- (window-width) (length prompt) space))
            (setq text
               (concat "…"
                 (substring text (- point (- (window-width) (length prompt) space))))))
        (if (> (length text) (- (window-width) (length prompt) (- space 1)))
            (setq text
               (concat (substring text 0 (- (window-width) (length prompt) (- space 1)))
                        (if (not end-of-line) "…")))))

      (with-current-buffer current-buffer
        (nano-command--update prompt text))
      (force-mode-line-update))))


(defun nano-command (&optional prompt hook message)
  "Read user-input from the mode-line using the given PROMPT. If a HOOK is provided, it is called on on the command buffer. If a MESSAGE is provided, it is displayed in the echo area."
  
  (interactive)
  (let* ((message (or message ""))
         (saved-mode-line mode-line-format)
         (saved-cursor-type cursor-type)
         (saved-header-line header-line-format)

         (cookie (face-remap-add-relative 'header-line
             :foreground (face-attribute 'nano-command-face :foreground nil 'default)
             :background (face-attribute 'nano-command-face :background nil 'default)
             :underline nil :overline nil
             :height (face-attribute 'nano-command-face :height nil 'default)
             :weight (face-attribute 'nano-command-face :weight nil 'default)
             :box (face-attribute 'nano-command-face :box nil 'default)))
         (command nil)
         (current-buffer (current-buffer))
         (current-window (selected-window))
         (prompt (concat
                  (if (boundp 'nano-modeline-space-top)
                      (propertize " " 'display `(raise ,nano-modeline-space-top))
                    "")
                  (or prompt "Quick command")
                  (if (boundp 'nano-modeline-space-bottom)
                      (propertize " " 'display `(raise ,nano-modeline-space-bottom))
                    "")))
         (prompt (concat (propertize prompt 'face 'nano-command-prompt-face)
                         " "))
         (command-buffer (get-buffer-create " *nano-command*")))
    
    ;; To make sure to remove the relative face
    (unwind-protect
        (setq-local cursor-type nil)
        (catch 'break
          (with-current-buffer command-buffer
            ;; Clear previous buffer content (if no buffer was given)
            (let ((inhibit-read-only t))
              (erase-buffer))
            ;; Call hook if any
            (when hook
              (funcall hook))

            (nano-command--process current-buffer command-buffer prompt)

            ;; Main loop where we read key sequences until RET or ESC is pressed
            (while t
              (let* ((event (read-key-sequence message))
                     (key (key-description event)))

                ;; Command enter
                (when (string= key "RET")
                  (setq command (buffer-substring (point-min) (point-max)))
                  (if (> (length command) 0)
                      (throw 'break command)
                    (throw 'break nil)))

                ;; Command abort
                (when (string= key "C-g")
                  (with-current-buffer command-buffer
                    (setq command nil)
                    (if (region-active-p)
                        (deactivate-mark)
                      (throw 'break nil))))

                ;; Execute key sequence in command buffer
                (condition-case error
                    (unless (string= key "C-g")
                      ;; Mouse wheel events are executed inside the current buffer
                      (if (or (string= key "<wheel-up>") (string= key "<wheel-down>")
                              (string= key "<wheel-left>") (string= key "<wheel-right>"))
                          (with-current-buffer current-buffer
                            (set-window-buffer current-window current-buffer t)
                            (mwheel-scroll (aref event 0)))

                        ;; Other key are executed in the command buffer
                        (with-current-buffer command-buffer
                          (set-window-buffer current-window command-buffer t)
                          (execute-kbd-macro (kbd key)))))
                  ((beginning-of-buffer end-of-buffer text-read-only)))
                
                (set-window-buffer current-window current-buffer t)
                (nano-command--process current-buffer command-buffer prompt)))))

      ;; Command entered or aborted: restore mode line
      (with-current-buffer current-buffer
        (setq-local mode-line-format saved-mode-line)
        (setq-local cursor-type saved-cursor-type)
        (setq-local header-line-format saved-header-line)
        (face-remap-remove-relative cookie)
        (force-mode-line-update))
      (kill-buffer command-buffer)
      (switch-to-buffer current-buffer))
      command))


;; ----------------------------------------------------------------------------
;; (set-face-attribute 'nano-command-face nil
;;                     :background (face-background 'nano-subtle)
;;                     :box nil)
;; (set-face-attribute 'nano-command-prompt-face nil
;;                     :foreground (face-foreground 'default)
;;                     :background (face-background 'default)
;;                     :box `(:line-width -1
;;                            :color ,(face-foreground 'nano-default)
;;                            :style none))
;; (set-face-attribute 'nano-command-region-face nil
;;                     :background "#ffffd0")

;; (set-face-attribute 'nano-command-cursor-face nil
;;                      :foreground (face-foreground 'nano-default-i)
;;                      :background (face-background 'nano-default-i))

