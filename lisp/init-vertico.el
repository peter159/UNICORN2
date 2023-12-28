;;; init-vertico.el ---                                  -*- lexical-binding: t; -*-

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

(use-package vertico
  :ensure nil
  :quelpa (vertico :repo "minad/vertico"
		   :fetcher github)
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize t
        vertico-count 17
        vertico-cycle t)
  ;; (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  ;; (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  ;; (define-key vertico-map [backspace] #'vertico-directory-delete-char)
  )

(use-package nerd-icons-completion
  :ensure t
  :when (icons-displayable-p)
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package orderless
  :ensure t
  :demand t
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))
  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))
  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq
   ;; completion-styles '(orderless partial-completion)
   completion-styles '(orderless partial-completion basic)
   completion-category-defaults nil
   ;; Enable partial-completion for files.
   ;; Either give orderless precedence or partial-completion.
   ;; Note that completion-category-overrides is not really an override,
   ;; but rather prepended to the default completion-styles.
   ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
   completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                   ;; enable initialism by default for symbols
                                   (command (styles +orderless-with-initialism))
                                   (variable (styles +orderless-with-initialism))
                                   (symbol (styles +orderless-with-initialism)))
   orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
   orderless-style-dispatchers '(+orderless-dispatch))
  )

(use-package consult
  :ensure nil
  :quelpa (consult :repo "minad/consult"
		 :fetcher github)
  :bind (("C-s"   . consult-line))
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (if sys/win32p
      (progn
        (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
        (add-to-list 'process-coding-system-alist '("explorer" gbk . gbk))
        (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))))
  :config
  (setq ;; consult-project-root-function #'doom-project-root
   consult-narrow-key "<"
   consult-project-function (lambda (_) (projectile-project-root))
   consult-line-numbers-widen t
   consult-async-min-input 2
   consult-async-refresh-delay  0.15
   consult-async-input-throttle 0.2
   consult-async-input-debounce 0.1)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key "M-.")
  (consult-customize
   consult-theme
   :preview-key (list :debounce 0.5 'any))
  (advice-add #'multi-occur :override #'consult-multi-occur))

(use-package consult-dir
  :ensure nil
  :quelpa (consult-dir :repo "karthink/consult-dir"
		       :fetcher github)
  :bind (([remap list-directory] . consult-dir)))

(use-package consult-flycheck
  :ensure nil
  :quelpa (consult-flycheck :repo "minad/consult-flycheck"
			    :fetcher github)
  :after (consult flycheck))

(use-package consult-yasnippet
  :ensure nil
  :quelpa (consult-yasnippet :repo "mohkale/consult-yasnippet"
			     :fetcher github)
  :after (consult yasnippet))

(use-package consult-projectile
  :ensure nil
  :quelpa (consult-projectile :repo "emacsmirror/consult-projectile"
			      :fetcher github)
  )

(use-package embark
  :ensure t
  :init
  (setq which-key-use-C-h-commands nil
        ;; press C-h after a prefix key, it shows all the possible key bindings and let you choose what you want
        prefix-help-command #'embark-prefix-help-command)
  (setq
   embark-verbose-indicator-display-action
   '((display-buffer-at-bottom)
     ;; (window-parameters (mode-line-format . none))
     (window-height . fit-window-to-buffer))))

(use-package marginalia
  :ensure nil
  :quelpa (marginalia :repo "minad/marginalia"
		      :fetcher github)
  :hook (after-init . marginalia-mode))

(use-package embark-consult
  :ensure t
  :quelpa (embark-consult :repo "emacs-straight/embark-consult"
			  :fetcher github)
  :after (embark consult)
  :demand
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;; edit the text in the grep buffer after typing C-c C-p
(use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(provide 'init-vertico)
(message "init-vertico loaded in %.2f seconds ..." (get-time-diff time-marked))
