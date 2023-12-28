;;; init-misc.el ---                                 -*- lexical-binding: t; -*-

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

(use-package default-text-scale
  :ensure t
  :init
  (default-text-scale-mode))

(use-package beacon
  :ensure t
  :custom
  (beacon-color "yellow")
  :init
  (beacon-mode 1))

(provide 'init-misc)
(message "init-misc loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-misc.el ends here
