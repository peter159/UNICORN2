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

(use-package gptel
  :ensure t
  :config
  (setq-default gptel-backend
		(gptel-make-azure
		 "gpt35-turbo"		;Name, whatever you'd like
		 :protocol "https"		;optional -- https is the default
		 :host "linyi2.openai.azure.com"
		 :endpoint "/openai/deployments/gpt35-azure-turbo/completions?api-version=2023-05-15" ;or equivalent
		 :stream t		;Enable streaming responses
		 :models '("gpt-3.5-turbo"))
		)
  )

(provide 'init-gpt)
(message "init-gpt loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-org.el ends here
