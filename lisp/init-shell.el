;;; init-shell.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <lipe6002@SHA-LPC-03254>
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

(defun open-mintty-terminal ()
  (interactive)
  (if (eq window-system 'w32)
      (progn
	(condition-case nil
	    (w32-shell-execute "runas" "c:\\msys64\\usr\\bin\\mintty.exe" "/bin/env MSYSTEM=64 CHERE_INVOKING=1 /bin/bash --login -i")
	  (error (w32-shell-execute "runas" "d:\\msys64\\usr\\bin\\mintty.exe" "/bin/env MSYSTEM=64 CHERE_INVOKING=1 /bin/bash --login -i"))) ;eval either is ture
	)
    (if (string-match-p "Microsoft" (shell-command-to-string "uname -a"))
	(start-process "" nil "/mnt/c/Users/linyi/AppData/Local/Microsoft/WindowsApps/wt.exe") ;;open windows terminal
      (start-process "" nil "/usr/bin/gnome-terminal")) ;open ubuntu terminal
    ))
;;

;; (define-key emacs-lisp-mode-map (kbd "C-S-c") 'open-mintty-terminal)
(define-key global-map (kbd "C-S-c") 'open-mintty-terminal)

;; (use-package shell-here
;;   :ensure t
;;   :bind (:map shell-mode-map
;; 	      ("C-l" . comint-clear-buffer)))


(use-package vterm
  :ensure t
  :preface
  (defun vterm--kill-vterm-buffer-and-window (process event)
    "Kill buffer and window on vterm process termination."
    (when (not (process-live-p process))
      (let ((buf (process-buffer process)))
	(when (buffer-live-p buf)
          (with-current-buffer buf
            (kill-buffer)
            (ignore-errors (delete-window))
            (message "VTerm closed."))))))
  :bind
  :config
  (evil-define-key 'insert vterm-mode-map (kbd "C-p") 'vterm-send-up)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n") 'vterm-send-down)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a") 'vterm-send-C-a)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e") 'vterm-send-C-e)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k") 'vterm-send-C-k)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d") 'vterm-send-C-d)
  (add-hook 'vterm-mode-hook (lambda()
			       (set-process-sentinel (get-buffer-process (buffer-name))
						     #'vterm--kill-vterm-buffer-and-window))))
(use-package multi-vterm
  :ensure t)

(defun my-shell-here()
  "open shell here and automatically close window when quiting the shell"
  (interactive)
  (if (eq window-system 'w32)
      (message "not ready for windows")
    (multi-vterm-dedicated-toggle)
    )
  )

(provide 'init-shell)
;;; init-shell.el ends here
