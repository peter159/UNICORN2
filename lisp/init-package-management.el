;;; init-package-management.el ---                   -*- lexical-binding: t; -*-

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

;; install `use-package' if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; set for package refresh
(unless package-archive-contents
  (package-refresh-contents))

;; should set before loading `use-package'
(eval-and-compile
  ;; (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

;; (message "    ---- 1/6 of init-package-management loaded using '%.2f' seconds ..." (get-time-diff time-marked))
;; (mark-time-here)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;; (message "    ---- 1/3 of init-package-management loaded using '%.2f' seconds ..." (get-time-diff time-marked))
;; (mark-time-here)

;; install and load quelpa
(use-package quelpa
  :ensure t
  :init
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-update-melpa-p nil)
  (setq quelpa-checkout-melpa-p nil))

;; (message "    ---- 1/2 of init-package-management loaded using '%.2f' seconds ..." (get-time-diff time-marked))
;; (mark-time-here)

(use-package quelpa-use-package
  :ensure t
  :init
  (require 'quelpa-use-package)
  ;; (quelpa-use-package-activate-advice)
  )

;; (message "    ---- 2/3 of init-package-management loaded using '%.2f' seconds ..." (get-time-diff time-marked))
;; (mark-time-here)

(use-package diminish :ensure t)
(use-package bind-key :ensure t)

(provide 'init-package-management)
(message "init-package-management loaded in '%.2f' seconds ..." (get-time-diff time-marked))
