;;; init-version-control.el ---                      -*- lexical-binding: t; -*-

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

(use-package magit
  :ensure t
  :config
  ;; show tasks
  (use-package magit-todos
    :ensure t
    :hook (after-init . magit-todos-mode))
  ;; Open github/gitlab/bitbucket page
  (use-package browse-at-remote
    :ensure t
    :bind (:map vc-prefix-map
		("B" . browse-at-remote))))

(provide 'init-version-control)
(message "init-version-control loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-version-control.el ends here
