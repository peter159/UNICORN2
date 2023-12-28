;;; init-c-c++.el ---                                -*- lexical-binding: t; -*-

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
;; if need dap mdoe for cpp, refer to here: https://emacs-lsp.github.io/dap-mode/page/configuration/
;; ref: https://tuhdo.github.io/c-ide.html
;; debug usage
;; C-c C-c g++ -g -o test myproj.cpp
;; C-c C-b gdb -i=mi test
;; https://linuxhint.com/c_emacs_configuration/ FIXME config this when available

;; 

;;; Code:

(mark-time-here)


(use-package smart-semicolon
  :ensure t
  :defer t
  :hook ((c-mode-common . smart-semicolon-mode)))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
  :config
  (add-hook 'cmake-mode-hook (lambda()
                               (add-to-list (make-local-variable 'company-backends)
                                            'company-cmake))))
(use-package google-c-style
  :ensure t
  :init
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

;; run dap-cpptools-setup to setup automatically
;; (require 'dap-cpptools)

(provide 'init-c-c++)
(message "init-c-c++ loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-c-c++.el ends here
