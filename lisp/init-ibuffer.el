;;; init-ibuffer.el ---                              -*- lexical-binding: t; -*-

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

(use-package ibuffer
  :ensure nil
  :preface
  (defun unicorn/ibuffer-advance-motion (direction)
    (forward-line direction)
    (beginning-of-line)
    (if (not (get-text-property (point) 'ibuffer-filter-group-name))
        t
      (ibuffer-skip-properties '(ibuffer-filter-group-name)
                               direction)
      nil))
  (defun unicorn/ibuffer-previous-line (&optional arg)
    "Move backwards ARG lines, wrapping around the list if necessary."
    (interactive "P")
    (or arg (setq arg 1))
    (let (err1 err2)
      (while (> arg 0)
        (cl-decf arg)
        (setq err1 (unicorn/ibuffer-advance-motion -1)
              err2 (if (not (get-text-property (point) 'ibuffer-title))
                       t
                     (goto-char (point-max))
                     (beginning-of-line)
                     (ibuffer-skip-properties '(ibuffer-summary
                                                ibuffer-filter-group-name)
                                              -1)
                     nil)))
      (and err1 err2)))
  (defun unicorn/ibuffer-next-line (&optional arg)
    "Move forward ARG lines, wrapping around the list if necessary."
    (interactive "P")
    (or arg (setq arg 1))
    (let (err1 err2)
      (while (> arg 0)
        (cl-decf arg)
        (setq err1 (unicorn/ibuffer-advance-motion 1)
              err2 (if (not (get-text-property (point) 'ibuffer-summary))
                       t
                     (goto-char (point-min))
                     (beginning-of-line)
                     (ibuffer-skip-properties '(ibuffer-summary ibuffer-filter-group-name ibuffer-title) 1) nil)
	      ))
      (and err1 err2)))
  (defun unicorn/ibuffer-next-group ()
    (interactive)
    (while (unicorn/ibuffer-next-line)))
  (defun unicorn/ibuffer-previous-group ()
    (interactive)
    (while (unicorn/ibuffer-previous-line)))
  (defun unicorn/ibuffer-visit-buffer ()
    (interactive)
    (ibuffer-visit-buffer)
    (kill-buffer "*Ibuffer*"))
  :commands (ibuffer-find-file
             ibuffer-current-buffer)
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  (define-key ibuffer-mode-map (kbd "j") 'unicorn/ibuffer-next-line)
  (define-key ibuffer-mode-map (kbd "k") 'unicorn/ibuffer-previous-line)
  (define-key ibuffer-mode-map (kbd "J") 'unicorn/ibuffer-next-group)
  (define-key ibuffer-mode-map (kbd "K") 'unicorn/ibuffer-previous-group)
  (define-key ibuffer-mode-map (kbd "RET") 'unicorn/ibuffer-visit-buffer))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :init (setq nerd-icons-ibuffer-icon t))

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :ensure t
  :functions nerd-icons-octicon
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix
        (if (display-graphic-p)
            (concat
             (nerd-icons-octicon "nf-oct-file_directory_fill"
                                 :face ibuffer-filter-group-name-face
                                 :v-adjust -0.05
                                 :height 1.25)
             " ")
          "Project: ")))

(provide 'init-ibuffer)
(message "init-ibuffer loaded in %.2f second ..." (get-time-diff time-marked))
;;; init-ibuffer.el ends here
