;;; leader-core-functions.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  <peter.linyi@DESKTOP-PMTGUNT>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(mark-time-here)

;; from https://gist.github.com/3402786
(defun unicorn/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))


;; https://tsdh.wordpress.com/2007/03/28/deleting-windows-vertically-or-horizontally/
(defun unicorn/maximize-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun unicorn/maximize-vertically ()
  "Delete all windows above and below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-up) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-down) (error nil))
      (delete-window))))

(defun unicorn/evil-goto-definition-other-window ()
  "Jump to definition around point in other window."
  (interactive)
  (let ((pos (point)))
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (evil-goto-definition)))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun unicorn/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

(defun unicorn/hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings"
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

(defun unicorn/remove-dos-eol ()
  "Replace DOS eol CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unicorn/goto-dashboard ()
  "goto Home buffer"
  (interactive)
  (switch-to-buffer "*dashboard*"))

(defun unicorn/goto-scratch-buffer ()
  "goto Home buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

;; Revert buffer
(defun unicorn/revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer.")
  (ignore-errors
    (widen)
    (text-scale-increase 0)
    (if (fboundp 'fancy-widen)
        (fancy-widen)))
  (revert-buffer t t))
(bind-key "<f5>" #'unicorn/revert-current-buffer)
(if (eq system-type 'darwin)
    (bind-key "s-r" #'unicorn/revert-current-buffer))

(defun unicorn/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun unicorn/consult-jump-in-buffer ()
  "Jump in buffer with `consult-imenu' or `consult-org-goto' if in org-mode"
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode) 'consult-org-goto)
    (t 'consult-imenu))))

(defun unicorn/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current filename."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        ;; the buffer is visiting a file
        (let* ((dir (file-name-directory filename))
               (new-name (read-file-name "New name: " (if arg dir filename))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p
                               (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (when (projectile-project-p)
                   (call-interactively #'projectile-invalidate-cache))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name)))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                name)
                               'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-name (read-string "New buffer name: ")))
                   (while (get-buffer new-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?")
                                  new-name))
                         (setq new-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-name)
                   (message "Buffer '%s' successfully renamed to '%s'"
                            name new-name)))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

(defun unicorn--file-path ()
  "Retrieve the file path of the current buffer.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

(defun unicorn/copy-directory-path ()
  "Copy and show the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (unicorn--directory-path))
      (message "%s" (kill-new directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun unicorn/copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (unicorn--file-path))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun unicorn/copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let (file-name (file-name-nondirectory (unicorn--file-path)))
      (message "%s" (kill-new file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun unicorn/projectile-copy-file-path ()
  "Copy and show the file path relative to project root."
  (interactive)
  (if-let (file-path (unicorn--projectile-file-path))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun unicorn/pop-eshell (arg)
  "Pop a shell in a side window.
Pass arg to ‘shell’."
  (interactive "P")
  (select-window
   (display-buffer-in-side-window
    (save-window-excursion
      (let ((prefix-arg arg))
        (call-interactively #'eshell))
      (current-buffer))
    '((side . right)
      (window-width . fit-window-to-buffer)))))

(defun unicorn/projectile-pop-eshell ()
  "Open a term buffer at projectile project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'unicorn/pop-eshell)))


;; Recompile site-lisp directory
(defun recompile-site-lisp ()
  "Recompile packages in site-lisp directory."
  (interactive)
  (let ((dir (concat user-emacs-directory "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

(defun unicorn/find-dotfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
  ;; (find-file-existing (concat user-emacs-directory "init.el")))

(defun unicorn/find-custom-file ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing (concat user-emacs-directory "custom.el")))

(defun unicorn/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window)))
     nil t)))

(defun unicorn/frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (condition-case nil
      (delete-frame nil 1)
    (error
     (make-frame-invisible nil 1))))

(defun unicorn//setup-default-key-name (key desc)
  (which-key-add-key-based-replacements
    (format "%s %s" unicorn-evil-leader-key key) desc)
  (which-key-add-key-based-replacements
    (format "%s %s" unicorn-evil-major-leader-insert-default-key key) desc))

(defun unicorn//setup-major-mode-key-name (key name)
  (which-key-add-key-based-replacements (format "%s m%s" unicorn-evil-leader-key key) name)
  (which-key-add-key-based-replacements (format "%s m%s" unicorn-evil-major-leader-insert-default-key key) name)
  (which-key-add-key-based-replacements (format ", %s" key) name))

(defun windows-to-linux-path ()
  "Prompts the user for a Windows path and converts it to a Linux path."
  (interactive)
  (let* ((path (read-string "Enter Windows path: "))
         (linux-path (replace-regexp-in-string "\\\\" "/" path)))
    (message "Linux path: %s" (concat "/mnt/" (downcase (substring linux-path 0 1)) (substring linux-path 2)))
    (kill-new (concat "/mnt/" (downcase (substring linux-path 0 1)) (substring linux-path 2)))))

(defun symbol-outline-or-imenu-list-toggle ()
  "Toggle `symbols-outline-mode' or `imenu-list-mode' based on whether `lsp-mode' or `symbols-outline-mode' is active."
  (interactive)
  ;; ;FIXME: fix for ess-r-mode with symbols-outline-mode
  (if (or (and (bound-and-true-p lsp-mode) (not (eq major-mode 'ess-r-mode))) (get-buffer-window "*Outline*" t))
      (symbols-outline-smart-toggle)
    (imenu-list-smart-toggle)))

(provide 'leader-core-functions)
(message "leader-core-functions loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; core-functions.el ends here
