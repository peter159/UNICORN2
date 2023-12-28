;;; init-project.el ---                              -*- lexical-binding: t; -*-

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

(use-package projectile
  :ensure t
  :diminish
  :bind (:map projectile-mode-map
              ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix "")
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)
  :config
  (projectile-update-mode-line)         ; Update mode-line at the first time
  ;; Use the faster searcher to handle project files:
  ;; ripgrep `rg'
  (let ((command
         (when
             (executable-find "rg")
           (let ((rg-cmd ""))
             (dolist (dir projectile-globally-ignored-directories)
               (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
             (concat "rg -0 --files --color=never --hidden" rg-cmd)))
         ))
    (setq projectile-generic-command command))
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching nil)
  ;; FIXME: too slow while getting submodule files on Windows
  (setq projectile-git-submodule-command nil)
  )

(provide 'init-project)
(message "init-project loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-project.el ends here
