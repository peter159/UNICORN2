;; init.el --- MainEntry                            -*- lexical-binding: t; -*-

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

;;; require package manager, config archives source and initialize all
(require 'package)

;; set http proxy, not need when set `git config --global http.proxy' in terminal
(defvar global-httpproxy "192.168.1.12:12333")
(if (string= (system-name) "Ubun-born")
    (setq global-httpproxy "127.0.0.1:12333"))
(setenv "http_proxy" (concat "http://" global-httpproxy))
(setenv "https_proxy" (concat "https://" global-httpproxy))
(setq url-proxy-services `(("http" . ,global-httpproxy)
			   ("https" . ,global-httpproxy)
			   ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))

;; ;; use mirror
(setq package-check-signature nil)	; to avoid signature fail for package
(setq-default package-archives '(
				 ("gnu"    .  "https://elpa.gnu.org/packages/")
				 ("melpa"  .  "https://melpa.org/packages/")
				 ("nongnu" .  "https://elpa.nongnu.org/nongnu/")
				 ("org"    .  "https://orgmode.org/elpa/")
				 ))

;; Initialize packages unless it's done
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; ;; Display the total loading time in the minibuffer
(defun display-startup-echo-area-message ()
  "Display startup echo area message."
  (message "Initialized in %s" (emacs-init-time)))

;; (profiler-cpu-start)
;; (profiler-memory-start)

;; requre features from lisp
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; ;; add support for svg, WORKAROUND for lsp-treemacs reporting svg error
;; or build with --with-cairo-xcb
(add-to-list 'image-types 'svg)

(require 'init-custom-vars)
(require 'init-general-functions)
(require 'init-package-management)
(require 'init-default)
(require 'init-font)
(require 'init-ui)

(require 'init-evil)
(require 'init-corfu)
(require 'init-dired)
(require 'init-tool)
(require 'init-vertico)
(require 'init-ibuffer)
(require 'init-window)
(require 'init-layout)

(require 'init-highlight)
(require 'init-version-control)
(require 'init-project)
(require 'init-yasnippet)
(require 'init-treemacs)

(require 'init-program-basis)
;; (require 'init-flycheck)
(require 'init-flymake)
(require 'init-treesit)
(require 'init-elisp)
;; (require 'init-lsp)
(require 'init-eglot)
(require 'init-lsp-python)
(require 'init-lsp-ess)
;; (require 'init-lsp-golang)
(require 'init-c-c++)
(require 'init-java)
(require 'init-org)
(require 'init-gpt)
(require 'init-dockerfile)
(require 'init-ts)
;; (require 'init-html)
(require 'init-markdown)
(require 'init-json)
(require 'init-yaml)
(require 'init-sql)

(require 'init-shell)
(require 'init-misc)

(require 'leader-core-functions)
(require 'leader-key-binding)

(when (file-exists-p custom-file)
  (load-file custom-file))
