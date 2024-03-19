;;; init-company.el ---                              -*- lexical-binding: t; -*-

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

(defun nasy/orderless-dispatch-flex-first (_pattern index _total)
  "orderless-flex for corfu."
  (and (eq index 0) 'orderless-flex))

(defun nasy/setup-corfu ()
  "Setup corfu."
  (setq-local orderless-matching-styles '(orderless-flex)
              orderless-style-dispatchers nil)
  (add-hook 'orderless-style-dispatchers #'nasy/orderless-dispatch-flex-first nil 'local))

;; use corfu instead
(use-package nerd-icons :ensure t)

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu
  :ensure nil
  :quelpa (corfu :repo "minad/corfu"
		 :fetcher github)
  :bind (:map corfu-map
	      ("C-M-m" . corfu-move-to-minibuffer))
  :init
  (setq corfu-cycle t
        corfu-auto t
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-preview-current nil
        corfu-auto-delay 0.1
        corfu-auto-prefix 1
	corfu-on-exact-match nil
        )
  (when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3))))))
  (global-corfu-mode)
  (defun corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))
  (defun corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))
  (define-key corfu-map [remap move-beginning-of-line] #'corfu-beginning-of-prompt)
  (define-key corfu-map [remap move-end-of-line] #'corfu-end-of-prompt)
  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none) ;; we use Corfu!
    (defun unicorn/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless
    (add-hook 'lsp-completion-mode-hook #'unicorn/lsp-mode-setup-completion)))

;; M-x kind-icon-preview-all to reset and preview all icons after installation
(use-package kind-icon
  :ensure t
  :quelpa (kind-icon :fetcher github
		     :repo "jdtsmith/kind-icon"
		     :files ("*.el"))
  :after corfu
  :init
  (require 'kind-icon)
  ;; to compute blended backgrounds correctly
  (when (icons-displayable-p)
    (setq kind-icon-default-face 'corfu-default))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ;; Use dabbrev with Corfu!
(use-package dabbrev
  :ensure t
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package cape
  :ensure nil
  :quelpa (cape :repo "minad/cape"
		:fetcher github)
  :preface
  (defun unicorn/set-lsp-capfs ()
    (setq-local completion-at-point-functions
		(list (cape-capf-super
		       #'yasnippet-capf
		       #'lsp-completion-at-point
		       )
		      #'cape-file
		      #'cape-dabbrev)))
  (defun unicorn/eglot-capf-setup ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)) ;; Configure orderless
    (setq-local completion-at-point-functions
    		(list
                 #'yasnippet-capf
                 #'eglot-completion-at-point
                 #'cape-file
    		 #'cape-dabbrev
                 )))
  :hook
  (lsp-completion-mode . unicorn/set-lsp-capfs)
  (eglot--managed-mode . unicorn/eglot-capf-setup)
  :init
  (setq cape-dabbrev-min-length 2
	cape-dabbrev-check-other-buffers nil)
  :config
  ;; 默认用这三个补全后端
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(provide 'init-corfu)
(message "init-corfu loaded in '%.2f' seconds" (get-time-diff time-marked))
;;; init-company.el ends here
