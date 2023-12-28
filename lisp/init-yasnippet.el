;;; init-yasnippet.el ---                            -*- lexical-binding: t; -*-

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

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :ensure nil
  :quelpa
  (yasnippet-snippets :fetcher github
    		      :repo "AndreaCrotti/yasnippet-snippets"
    		      :files ("*"))
  )

(use-package yasnippet-capf
  :ensure nil
  :quelpa
  (yasnippet-capf :fetcher github
    		  :repo "elken/yasnippet-capf"
    		  :files ("*"))
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(provide 'init-yasnippet)
(message "init-yasnippet loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-yasnippet.el ends here
