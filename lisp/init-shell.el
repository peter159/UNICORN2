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

(defun unicorn/wsl-p ()
  "Return non-nil if running under WSL."
  (or (getenv "WSL_DISTRO_NAME")
      (let ((os-release "/proc/sys/kernel/osrelease"))
        (when (and (eq system-type 'gnu/linux)
                   (file-readable-p os-release))
          (with-temp-buffer
            (insert-file-contents os-release)
            (goto-char (point-min))
            (re-search-forward "Microsoft" nil t))))))

(defun open-mintty-terminal ()
  (interactive)
  (cond
   ((eq window-system 'w32)
    (let ((paths '("c:\\msys64\\usr\\bin\\mintty.exe"
                   "d:\\msys64\\usr\\bin\\mintty.exe"))
          (args "/bin/env MSYSTEM=64 CHERE_INVOKING=1 /bin/bash --login -i")
          (target nil))
      (dolist (path paths)
        (when (and (not target) (file-exists-p path))
          (setq target path)))
      (if target
          (w32-shell-execute "runas" target args)
        (user-error "mintty.exe not found in MSYS64 paths"))))
   ((unicorn/wsl-p)
    (let* ((wt-path "/mnt/c/Users/linyi/AppData/Local/Microsoft/WindowsApps/wt.exe")
           (root (and default-directory
                      (file-directory-p default-directory)
                      (locate-dominating-file default-directory "pyrightconfig.json")))
           (venv (when root
                   (let ((config (expand-file-name "pyrightconfig.json" root)))
                     (when (file-readable-p config)
                       (require 'json)
                       (let ((json-object-type 'hash-table)
                             (json-array-type 'list)
                             (json-key-type 'string))
                         (condition-case nil
                             (let* ((data (json-read-file config))
                                    (name (gethash "venv" data)))
                               (when (and (stringp name) (> (length name) 0))
                                 name))
                           (error nil)))))))
           (activate-cmd (when venv
                           (format "conda activate %s && exec zsh -i"
                                   (shell-quote-argument venv))))
           (wsl-available (or (executable-find "wsl.exe")
                              (file-executable-p "/mnt/c/Windows/System32/wsl.exe")))
           (distro (getenv "WSL_DISTRO_NAME"))
           (wsl-args (when (and activate-cmd wsl-available)
                       (append (list "wsl.exe")
                               (when (and distro (not (string= distro "")))
                                 (list "-d" distro))
                               (list "zsh" "-ic" activate-cmd))))
           (cmd (cond
                 ((file-exists-p wt-path) (list wt-path))
                 ((executable-find "wt.exe") (list "wt.exe"))
                 ((executable-find "cmd.exe") (list "cmd.exe" "/c" "start" "wt.exe"))
                 (t nil))))
      (if (and cmd wsl-args)
          (apply #'start-process "windows-terminal" nil (append cmd wsl-args))
        (if cmd
            (apply #'start-process "windows-terminal" nil cmd)
          (user-error "Windows Terminal not found")))))
   (t
    (let ((terminal (or (executable-find "gnome-terminal")
                        (executable-find "x-terminal-emulator")
                        (executable-find "konsole")
                        (executable-find "xfce4-terminal")
                        (executable-find "alacritty")
                        (executable-find "kitty"))))
      (if terminal
          (start-process "terminal" nil terminal)
        (user-error "No terminal program found"))))))
;;

;; (define-key emacs-lisp-mode-map (kbd "C-S-c") 'open-mintty-terminal)
(define-key global-map (kbd "C-S-c") 'open-mintty-terminal)


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
