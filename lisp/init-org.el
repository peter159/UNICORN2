;;; init-org.el ---                                  -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'init-const))

(defvar org-projectile-file "TODOs.org")

(use-package org
  :ensure t
  :commands (orgtbl-mode)
  :init
  (require 'org)
  (setq org-directory "~/org"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        ;; org-todo-keywords '((sequence "TODO(T)" "DOING(I)" "HANGUP(H)" "|" "DONE(D)" "CANCEL(C)")
        ;;                     (sequence "⚑(t)" "🏴(i)" "❓(h)" "|" "✔(d)" "✘(c)")) ;TODO font not showing correctly
	org-todo-keywords '((sequence "TODO(t)" "DOING(I)" "HANGUP(H)" "|" "DONE(D)" "CANCEL(C)"))
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning))
        org-pretty-entities t
	org-hide-emphasis-markers t
        org-log-done t
        org-startup-with-inline-images t
        org-image-actual-width nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        ;; this is consistent with the value of
        ;; `helm-org-headings-max-depth'.
        org-imenu-depth 8)

  (add-to-list 'org-export-backends 'md)

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
			       (R . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if (>= emacs-major-version 26)
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-ipython
    :ensure t
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list))

(use-package org-agenda
  :ensure nil
  :init
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-current-time-string "← now")
  (setq org-agenda-time-grid ;; Format is changed from 9.1
        '((daily today require-timed)
          (0900 01000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
          "-"
	  "────────────────")))

;; Pomodoro
(use-package org-pomodoro
  :ensure t
  :after org-agenda
  :bind (:map org-agenda-mode-map
              ("P" . org-pomodoro)))

(use-package org-bullets
  :ensure t
  :if (char-displayable-p ?◉)
  :hook (org-mode . org-bullets-mode))

(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-enable)
  :init
  (setq toc-org-max-depth 10))

(use-package org-projectile
  :defer t
  :commands (org-projectile-location-for-project)
  :preface
  (defun org-projectile/capture (&optional arg)
    (interactive "P")
    (if arg
	(org-projectile-project-todo-completing-read :empty-lines 1)
      (org-projectile-capture-for-current-project :empty-lines 1)))

  (defun org-projectile/goto-todos ()
    (interactive)
    (org-projectile-goto-location-for-project (projectile-project-name)))

  :init
  (with-eval-after-load 'org-capture
    (require 'org-projectile))

  :config
  (if (file-name-absolute-p org-projectile-file)
      (progn
        (setq org-projectile-projects-file org-projectile-file)
        (push (org-projectile-project-todo-entry :empty-lines 1)
              org-capture-templates))
    (org-projectile-per-project)
    (setq org-projectile-per-project-filepath org-projectile-file)))

;; Preview
(use-package org-preview-html
  :ensure t
  :diminish org-preview-html-mode)

;; Visually summarize progress
;; (use-package org-dashboard :ensure)

;; (use-package org-plus-contrib :ensure t)

(provide 'init-org)
(message "init-org loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-org.el ends here
