;;; evil-major-leader.el --- provides a shortcut to reach evil-leader's mode keymap    -*- lexical-binding: t; -*-

;; Copyright (C) 2019 by Peter Chou

;; Author:  Peter Chou <2747244153@qq.com>
;; Homepage: https://github.com/Peter-Chou/evil-major-leader
;; Version: 1.0.2
;; Package-Version: 20190613.71444
;; Package-Requires: ((evil "0") (evil-leader "0.4.3"))
;; Keywords: lisp evil

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:

;; evil-major-leader acts as a complement to evil-leader package

;;; Code:

(require 'evil)
(require 'evil-leader)

(defgroup evil-major-leader nil
  "<major-leader> support for evil."
  :group 'evil
  :prefix 'evil-major-leader/)

(defcustom evil-major-leader-default-key "\,"
  "The <major-leader> key, used to access evil-leader's mode keys."
  :type 'string
  :group 'evil-major-leader)

(defcustom evil-major-leader-mode-key "m"
  "the major mode key binded in evil-leader's <leader>"
  :type 'string
  :group 'evil-major-leader)

(defcustom evil-major-leader-insert-default-key "M-m"
  "The <leader> key, used to trigger evil-leader's default keys in evil insert mode."
  :type 'string
  :group 'evil-major-leader)

(defun evil-major-leader/get-mode-key-ascii ()
  "get ascii of mode key"
  (aref evil-major-leader-mode-key 0))

;;;###autoload
(define-minor-mode global-evil-major-leader-mode
  "Global minor mode for <major-leader> support."
  nil nil nil
  (if global-evil-major-leader-mode
      (add-hook 'evil-local-mode-hook #'evil-major-leader-mode t)
    (remove-hook 'evil-local-mode-hook #'evil-major-leader-mode t)))

;;;###autoload
(define-minor-mode evil-major-leader-mode
  "Minor mode to enable <major-leader> support."
  :init-value nil
  :keymap nil
  (let* ((no-prefix (read-kbd-macro evil-major-leader-default-key))
	 (evil-insert--leader-key (read-kbd-macro evil-major-leader-insert-default-key))
	 (mode--map (cdr (assoc major-mode evil-leader--mode-maps)))
	 (major-mode-map (cdr (assoc (evil-major-leader/get-mode-key-ascii) (cdr (assoc major-mode evil-leader--mode-maps)))))
	 (insert--mode-map (or mode--map evil-leader--default-map))
	 )
    (if evil-major-leader-mode
	(progn
          (define-key evil-motion-state-local-map no-prefix major-mode-map)
          (define-key evil-normal-state-local-map no-prefix major-mode-map)
	  (define-key evil-insert-state-local-map evil-insert--leader-key insert--mode-map)))))

;;;###autoload
(defun evil-major-leader/set-leader (key)
  "Set leader key to `key'"
  (let ((global-major-on global-evil-major-leader-mode)
        (local-major-on evil-major-leader-mode))
    (when local-major-on
      (evil-major-leader-mode -1))
    (when global-major-on
      (global-evil-major-leader-mode -1))
    (setq evil-major-leader-default-key key)
    (if global-major-on
        (global-evil-major-leader-mode 1)
      (when local-major-on
        (evil-major-leader-mode 1)))))

(provide 'evil-major-leader)
;;; evil-major-leader.el ends here
