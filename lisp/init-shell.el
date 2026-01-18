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
  :quelpa (vterm :repo "akermu/emacs-libvterm"
		 :fetcher github)
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
  ;; 新增：在 vterm 中发送“真实 ESC”（用于退出 Claude /status 等 TUI）
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-c C-c") #'vterm-send-escape)
  (add-hook 'vterm-mode-hook (lambda()
			       (set-process-sentinel (get-buffer-process (buffer-name))
						     #'vterm--kill-vterm-buffer-and-window))))
(with-eval-after-load 'vterm
  (with-eval-after-load 'evil
    (defvar-local vterm-evil--esc-timer nil)
    (defvar-local vterm-evil--esc-primed nil)
    (defun vterm-evil--esc-reset ()
      (setq vterm-evil--esc-primed nil)
      (when (timerp vterm-evil--esc-timer)
        (cancel-timer vterm-evil--esc-timer))
      (setq vterm-evil--esc-timer nil))
    (defun vterm-evil--esc-primed-p ()
      (when vterm-evil--esc-primed
        (vterm-evil--esc-reset)
        t))
    (defun vterm-evil--esc-prime (&optional secs)
      (setq vterm-evil--esc-primed t)
      (when (timerp vterm-evil--esc-timer)
        (cancel-timer vterm-evil--esc-timer))
      (setq vterm-evil--esc-timer
            (run-with-timer (or secs 0.25) nil #'vterm-evil--esc-reset)))
    (defun vterm-evil-esc ()
      (interactive)
      (vterm-send-escape)
      (if (vterm-evil--esc-primed-p)
          (evil-normal-state)
        (vterm-evil--esc-prime)))
    (defun vterm-evil-esc-normal ()
      (interactive)
      (if (vterm-evil--esc-primed-p)
          (evil-emacs-state)
        (vterm-evil--esc-prime)))
    (add-hook 'vterm-mode-hook (lambda () (evil-emacs-state)))
    (evil-define-key 'emacs  vterm-mode-map (kbd "<escape>") #'vterm-evil-esc)
    (evil-define-key 'insert vterm-mode-map (kbd "<escape>") #'vterm-evil-esc)
    (evil-define-key 'normal vterm-mode-map (kbd "<escape>") #'vterm-evil-esc-normal)))

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

(use-package dotenv-mode
  :ensure t
  :mode (("\\.env\\'" . dotenv-mode)
         ("\\.env\\..*\\'" . dotenv-mode)))

;; 在 WSL 中强制使用 Windows Chrome
(when (and (eq system-type 'gnu/linux)
           (getenv "WSL_DISTRO_NAME"))
  (setq browse-url-browser-function
        (lambda (url &optional _new-window)
          (start-process
           "windows-chrome"
           nil
           "cmd.exe"
           "/c"
           "start"
           "chrome"
           url))))

(provide 'init-shell)
;;; init-shell.el ends here
