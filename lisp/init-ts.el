;;; init-html.el ---                                 -*- lexical-binding: t; -*-

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
;; ref: https://ithelp.ithome.com.tw/articles/10202632
;; ref: https://ithelp.ithome.com.tw/articles/10205908

;;

;;; Code:

(mark-time-here)

;; Major mode for editing web templates
(use-package web-mode
  :ensure t
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\)$"
  :hook (web-mode . (lambda () (electric-operator-mode -1)))
  :config
  (setq electric-pair-mode t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  ;; (add-hook 'web-mode-hook (lambda () (electric-operator-mode -1))
  )


;; https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/
;; npm i -g typescript-language-server; npm i -g typescript
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :config
  (setq typescript-indent-level 2)
  ;; :hook
  ;; (typescript-ts-mode . js2-minor-mode)
  )

(use-package js2-mode
  :ensure t)

(use-package vue-ts-mode
  :ensure nil
  :mode ("\\.vue\\'" . vue-ts-mode)
  :quelpa
  (vue-ts-mode :fetcher github
	       :repo "8uff3r/vue-ts-mode")
  :hook
  (vue-ts-mode . (lambda() (electric-operator-mode -1)))
  )

(use-package emmet-mode
  :ensure t
  :hook (web-mode sgml-mode html-mode css-mode vue-ts-mode))


(provide 'init-ts)
(message "init-ts loaded in '%.2f' seconds" (get-time-diff time-marked))
;;; init-html.el ends here
