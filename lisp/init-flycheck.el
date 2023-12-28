;;; init-flycheck.el ---                             -*- lexical-binding: t; -*-

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

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :init
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "RET") 'flycheck-error-list-goto-error)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "j") 'flycheck-error-list-next-error)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "k") 'flycheck-error-list-previous-error)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-flake8-maximum-line-length 99)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(provide 'init-flycheck)
(message "init-flycheck loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-flycheck.el ends here
