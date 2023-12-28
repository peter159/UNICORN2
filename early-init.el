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

;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer noninteractive)

(setq byte-compile-warnings '(cl-functions))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  ;; 检查 site-lisp 文件夹是否存在，如果不存在则创建它
  (unless (file-exists-p "~/.emacs.d/site-lisp")
    (make-directory "~/.emacs.d/site-lisp"))
  ;; 检查 lisp 文件夹是否存在，如果不存在则创建它
  (unless (file-exists-p "~/.emacs.d/lisp")
    (make-directory "~/.emacs.d/lisp"))	
  ;; 检查目录是否存在，如果不存在则创建
  (unless (file-exists-p "~/.emacs.d/quelpa/melpa/recipes")
    (make-directory "~/.emacs.d/quelpa/melpa/recipes" t))
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp/init-shell" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
	  (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(update-load-path)
(add-subdirs-to-load-path)
(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

(if (and (fboundp 'native-comp-available-p)
	 (native-comp-available-p))
    (progn
      (message "Native compilation is available")
      ;; native-compile all Elisp files under a site-lisp/local directory
      (native-compile-async (expand-file-name "site-lisp" user-emacs-directory) 'recursively)
      (setq package-native-compile t
	    native-comp-async-report-warnings-errors nil
            ;; Make native compilation happens asynchronously
            native-comp-deferred-compilation nil))
  (message "Native complation is *not* available"))

(setq package-enable-at-startup nil)

(setq load-prefer-newer noninteractive)

(setq frame-inhibit-implied-resize t)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "data/eln-cache" user-emacs-directory)))
  (add-to-list 'native-comp-eln-load-path (expand-file-name "data/eln-cache" user-emacs-directory))
  )

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
