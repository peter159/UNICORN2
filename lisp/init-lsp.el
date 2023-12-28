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

;; optimize lsp-mode
(setenv "LSP_USE_PLISTS" "true")

(use-package lsp-mode
  :ensure t
  ;; :quelpa (lsp-mode:repo "emacs-lsp/lsp-mode"
  ;; 		   :fetcher github)
  :diminish
  :defines (lsp-clients-python-library-directories
            lsp-rust-server)
  :commands (lsp-enable-which-key-integration
             lsp-format-buffer
             lsp-organize-imports
	     lsp-ui-doc-enable
             lsp-install-server)
  :init
  (setq lsp-log-io nil
	gc-cons-threshold (* 6400 1024 1024)
	read-process-output-max (* 40 1024 1024) ;; @see https://github.com/emacs-lsp/lsp-mode#performance
	lsp-use-plists t			 ;;https://emacs-lsp.github.io/lsp-mode/page/performance/
	lsp-idle-delay 0.005
	)
  (setq lsp-keymap-prefix "C-c l"
	lsp-auto-guess-root nil
	lsp-keep-workspace-alive nil
	lsp-completion-provider t
	lsp-signature-auto-activate t	; show arg list and not document in eldoc with next 3 lines
	lsp-signature-render-documentation nil
	lsp-eldoc-enable-hover t
	lsp-headerline-breadcrumb-enable nil ;https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
	lsp-ui-doc-enable t
	lsp-enable-file-watchers nil
	lsp-enable-folding nil
	lsp-enable-indentation nil
	lsp-enable-on-type-formatting nil
	lsp-enable-symbol-highlighting nil)
  :hook (
	 (prog-mode . (lambda ()
			(unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
			  (lsp-deferred))))
	 (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)))
	 (go-mode . lsp-deferred)
	 (typescript-mode . lsp-deferred)
	 )
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . #'lsp-find-definition)
              ([remap xref-find-references] . #'lsp-ui-peek-find-references)
	      )
  :config
  (with-eval-after-load 'lsp-mode
    (defun unicorn/lsp-mode-setup-completion()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	    '(flex)))			 ; Configure flex
    (setq lsp-completion-provider :none) ; we use Corful!
    (add-hook 'lsp-completion-mode-hook #'unicorn/lsp-mode-setup-completion)
    )
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-doc-mode)

(use-package consult-lsp
  :ensure t
  :after lsp-mode
  :bind (:map lsp-mode-map
              ("M-." . consult-lsp-symbols)))

(use-package lsp-treemacs
  :ensure t
  :preface
  (defun toggle-lsp-treemacs-symbols ()
    "Toggle the lsp-treemacs-symbols buffer."
    (interactive)
    (if (get-buffer "*LSP Symbols List*")
	(kill-buffer "*LSP Symbols List*")
      (progn (lsp-treemacs-symbols)
             (other-window -1))))
  :commands lsp-treemacs-errors-list
  :config
  (define-key prog-mode-map (kbd "<f7>") 'toggle-lsp-treemacs-symbols)
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

;; use for specified language
;; python with lsp mode
(use-package lsp-pyright
  :ensure t
  :hook ((python-mode python-ts-mode) . (lambda ()
					  (require 'lsp-pyright)
					  (lsp-deferred)))
  :init
  (setq lsp-pyright-typechecking-mode "off"
	lsp-pyright-venv-path (file-truename "~/miniconda3/envs")
	))

;; (use-package go-mode
;;   :ensure t
;;   :defer t
;;   :init
;;   (setq gofmt-command "goimports"
;; 	indent-tabs-mode t)
;;   :hook (go-mode . (lambda()
;; 		     (lsp-deferred)))
;;   )

;; c/c++ with lsp
(use-package cc-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist
	       `("\\.h\\'" . ,unicorn-default-mode-for-headers))
  (setq gdb-many-windows t
	gdb-show-main t)
  :hook ((c-mode c++-mode) . (lambda ()
			       "Format and add/delete imports."
			       (add-hook 'before-save-hook #'lsp-format-buffer t t)
			       (add-hook 'before-save-hook #'lsp-organize-imports t t)
			       ;; enable lsp
			       (lsp-deferred)))
  :config
  (require 'compile)
  (define-key c++-mode-map (kbd "C-c C-c") 'compile)
  (define-key c++-mode-map (kbd "C-c C-b") 'gdb)
  )

;; java with lsp-mode
(use-package lsp-java
  :hook (java-mode . (lambda ()
		       (require 'lsp-java)
		       (lsp-deferred)))
  :init
  (setq lsp-java-import-maven-enabled t
	lsp-java-implementations-code-lens-enabled t
	lsp-java-save-actions-organize-imports t
	;; latest jdtls requires java >= 11 to work
	lsp-java-java-path "/opt/jdk11/bin/java"
	lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx6G" "-Xms100m")
	;; Runtime name must be one of: “J2SE-1.5”, “JavaSE-1.6”, “JavaSE-1.7”, “JavaSE-1.8” etc
	;; lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
	;; 				   :path "/opt/jdk/")
	;; 				  (:name "JavaSE-11"
	;; 				   :path "/opt/jdk11/"
	;; :default t)]
	lsp-java-folding-range-enabled t)
  )

;; docker with lsp-mode
(use-package dockerfile-mode
  :ensure t
  :hook (dockerfile-mode . (lambda()
			     (lsp-deferred))))

;; web with lsp-mode
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'")
  :hook
  (html-mode . web-mode)
  ;; (web-mode . electric-spacing-mode)
  (web-mode . (lambda()
		(lsp-deferred)))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-css-colorization t)
  ;; (set-face-attribute 'web-mode-html-tag-face nil :foreground "royalblue")
  ;; (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "powderblue")
  ;; (set-face-attribute 'web-mode-doctype-face nil :foreground "lightskyblue")
  (setq web-mode-content-types-alist
        '(("vue" . "\\.vue\\'")))
  (define-key web-mode-map (kbd "M-n") 'web-mode-navigate)
  (define-key web-mode-map (kbd "<tab>") 'web-mode-fold-or-unfold)
  (define-key web-mode-map (kbd "M-o") 'browse-url-of-file)
  )

(provide 'init-lsp)
(message "init-lsp loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-lsp.el ends here
