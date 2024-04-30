;;; init-ui.el --- default user interface!!          -*- lexical-binding: t; -*-

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


;;; Code:

(mark-time-here)

;; hide menu-bar, tool-bar, scroll-bar and open with global line number mode
(menu-bar-mode -1)
(tool-bar-mode -1)
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(electric-pair-mode 1)
(setq-default make-backup-files nil)

(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1))

;; forbid emacs startup screen and make full screen default
(setq inhibit-splash-screen t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(defun make-full-screen()
  "make full screen"
  (modify-frame-parameters nil `((fullscreen . fullboth) (maximized . fullscreen))))
(add-hook 'after-init-hook 'global-display-fill-column-indicator-mode)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(add-hook 'focus-in-hook 'make-full-screen) ;make full screen even when server killed frame

;; display time
(display-time-mode t)
;; (setq display-time-24hr-format t) ;; void variable?
(setq display-time-day-and-date t)

;; switch some words to icons, like folder etc.
(use-package font-lock+
  :ensure nil
  :quelpa
  (font-lock+
   :repo "emacsmirror/font-lock-plus"
   :fetcher github))

(use-package doom-modeline
  :ensure nil
  :quelpa
  (doom-modeline
   :repo "seagle0128/doom-modeline"
   :fetcher github)
  :hook ((after-init . doom-modeline-mode)
	 (doom-modeline-mode . setup-custom-doom-modeline))
  :custom-face
  (doom-modeline-buffer-file ((t (:inherit font-lock-string-face :weight bold))))
  :config
  (progn
    (setq
     find-file-visit-truename t  ; display the real names for symlink files
     doom-modeline-height 25
     doom-modeline-lsp t
     doom-modeline-persp-name t
     doom-modeline-github t
     doom-modeline-time-icon t
     doom-modeline-time t
     ;; doom-modeline-buffer-file-name-style 'truncate-with-project ;cause stuck
     doom-modeline-buffer-file-name-style 'auto ;file-name
     doom-modeline-major-mode-color-icon t
     doom-modeline-enable-word-count t
     doom-modeline-minor-modes nil
     doom-modeline-env-version t
     doom-modeline-env-enable-python t)
    (doom-modeline-def-segment my-python-venv
      "The current python virtual environment state."
      (when (or (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode))
	(if (eq python-shell-virtualenv-root nil)
	    ""
	  (propertize
	   (let ((base-dir-name (file-name-nondirectory python-shell-virtualenv-root)))
	     (if (< 12 (length base-dir-name))
		 (format " (%s..)" (substring base-dir-name 0 12))
	       (format " (%s)" base-dir-name)))
	   'face (if (doom-modeline--active) 'doom-modeline-buffer-major-mode)))))
    (doom-modeline-def-modeline 'my-modeline-layout
      '(bar workspace-name window-number matches buffer-info remote-host buffer-position word-count parrot selection-info)
      '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl input-method time indent-info buffer-encoding process vcs))
    (defun setup-custom-doom-modeline ()
      (doom-modeline-set-modeline 'my-modeline-layout 'default))))

(use-package spacemacs-theme
  :ensure t)

(use-package modus-themes
  :ensure nil
  :init
  (setq
   modus-themes-bold-constructs t
   modus-themes-italic-constructs t
   modus-themes-org-blocks 'tinted-background
   ;; modus-themes-syntax '(yellow-comments green-strings)
   modus-themes-paren-match '(bold intense)
   modus-themes-mode-line '(accented borderless (height . 0.9))
   modus-themes-region '(bg-only)
   modus-themes-completions '((matches . (extrabold))
                              (selection . (semibold accented))
                              (popup . (accented intense)))
   modus-themes-headings ; this is an alist: read the manual or its doc string
   '((1 . (rainbow overline background 1.4))
     (2 . (rainbow background 1.3))
     (3 . (rainbow bold 1.2))
     (t . (semilight 1.1)))))

(load-theme 'modus-operandi-tinted t)
;; (load-theme 'modus-vivendi-tinted t)

(use-package display-line-numbers-mode
  :ensure nil
  :init
  (setq-default display-line-numbers-type 'absolute) ;relative
  (global-display-line-numbers-mode t))

(use-package diff-hl
  :ensure t
  :hook
  (after-init . global-diff-hl-mode))

(use-package hide-mode-line
  :ensure t
  :hook (((completion-list-mode
	   ;; completion-in-region-mode
	   flycheck-error-list-mode) . hide-mode-line-mode)))

(use-package transwin
  :ensure t
  :bind
  ("M-+" . transwin-inc)
  ("M-_" . transwin-dec)
  ("M-)" . transwin-toggle)
  :init
  (setq transwin-delta-alpha 5
        transwin-parameter-alpha 'alpha-background)
  :config
  (transwin-ask 80))

;; ;; https://github.com/cyrus-and/zoom, golden-ratio
;; (use-package zoom
;;   :ensure t
;;   :preface
;;   (defun size-callback ()
;;     (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
;;           (t                            '(0.5 . 0.5))))
;;   :hook
;;   (after-init . zoom-mode)
;;   :config
;;   (custom-set-variables
;;    '(zoom-size 'size-callback)))

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)

(provide 'init-ui)
(message "init-ui loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-ui.el ends here
