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

(use-package eglot
  :ensure t
  :init
  (setq eglot-send-changes-idle-time 0.2
	eglot-autoshutdown t
	eglot-connect-timeout 120
	eglot-ignored-server-capabilities '(:inlayHintProvider)
	eldoc-echo-area-use-multiline-p nil
	eglot-events-buffer-size 1
	eglot-server-programs '(
				((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))
				((ess-r-mode) . ("R" "--slave" "-e" "languageserver::run()"))
				((c++-mode c-mode c++-ts-mode c-ts-mode objc-mode) ("clangd"))
				((cmake-mode cmake-ts-mode) . ("cmake-language-server"))
				((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
				((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode)
				 . ("gopls"))
				((yaml-ts-mode yaml-mode) . ("yaml-language-server" "--stdio"))
				((dockerfile-mode dockerfile-ts-mode) . ("docker-langserver" "--stdio"))))
  :hook
  ((python-mode python-ts-mode) . eglot-ensure)
  ((ess-r-mode) . eglot-ensure)
  )

(use-package format-all
  :ensure t
  :commands format-all-mode
  :hook
  ;; (lsp-mode . format-all-mode)
  ;; (gfm-mode . format-all-mode)
  (prog-mode . format-all-mode)
  ;; (format-all-mode . format-all-ensure-formatter)
  :config
  (setq-default format-all-formatters '(("C"     (astyle "--mode=c"))
                                        ("Shell" (shfmt "-i" "4" "-ci"))
					))
  (global-set-key (kbd "M-f") 'format-all-buffer))

;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(provide 'init-eglot)
(message "init-eglot loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-lsp.el ends here
