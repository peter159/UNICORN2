;;; init-custom-vars.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  linyi

;; Author: linyi <linyi@ubu-born-0>
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

(defvar unicorn-evil-major-leader-insert-default-key "M-m"
  "Evil leader key in evil insert mode.")

(defvar unicorn-lsp-active-modes '(
				   c-mode
				   c++-mode
				   python-mode
				   python-ts-mode
				   java-mode
				   scala-mode
				   go-mode
				   sh-mode
				   vue-mode
				   )
  "Primary major modes of the lsp activated layer.")

(defvar unicorn-default-mode-for-headers 'c++-mode
  "default default mode for .h header files, Can be `c-mode' or `c++-mode'")

(defalias 'yes-or-no-p 'y-or-n-p)

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and (or (featurep 'nerd-icons)
	   (require 'nerd-icons nil t))))

(provide 'init-custom-vars)
