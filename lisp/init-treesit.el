;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-

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

;; M-x treesit-auto-install-all
(use-package treesit-auto
  :ensure t
  :demand t
  :init
  (setq treesit-auto-install 'prompt
        treesit-font-lock-level 4)
  :config
  (setq treesit-language-source-alist
        '((vue "https://github.com/ikatyang/tree-sitter-vue")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	  ))
  (global-treesit-auto-mode))

(provide 'init-treesit)
(message "init-treesit loaded in '%.2f' seconds ..." (get-time-diff time-marked))
