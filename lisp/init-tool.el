;;; init-tool.el ---                                 -*- lexical-binding: t; -*-

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

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :bind (:map help-map
	      ("C-h" . which-key-C-h-dispatch))
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-idle-delay 0.2)
  (setq which-key-separator " → ")
  (setq which-key-prefix-prefix "+")
  ;; Needed to avoid nil variable error before update to recent which-key
  (defvar which-key-replacement-alist nil)
  ;; Reset to the default or customized value before adding our values in order
  ;; to make this initialization code idempotent.
  (custom-reevaluate-setting 'which-key-replacement-alist)
  ;; Replace rules for better naming of functions
  (let ((new-descriptions
         ;; being higher in this list means the replacement is applied later
         '(("unicorn/\\(.+\\)" . "\\1")
	   ("unicorn/toggle-\\(.+\\)" . "\\1")
	   ("avy-goto-word-or-subword-1" . "avy word")
	   ("shell-command" . "shell cmd")
	   ("universal-argument" . "universal arg")
	   ("er/expand-region" . "expand region")
	   ("counsult-ripgrep" . "project rg")
	   ("evil-lisp-state-\\(.+\\)" . "\\1")
	   ("helm-mini\\|ivy-switch-buffer" . "list-buffers"))))
    (dolist (nd new-descriptions)
      ;; ensure the target matches the whole string
      (push (cons (cons nil (concat "\\`" (car nd) "\\'")) (cons nil (cdr nd)))
            which-key-replacement-alist)))
  )

(use-package imenu-list
  :ensure t
  :defer t
  :hook (imenu-list-major-mode . (lambda ()
				   (display-line-numbers-mode -1)
				   (hl-line-mode -1)))
  :init
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil)
  :config
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "d") 'imenu-list-display-entry)
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "r") 'imenu-list-refresh)
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "q") 'imenu-list-quit-window)
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "<tab>") 'hs-toggle-hiding)
  (evil-define-key 'normal imenu-list-major-mode-map [down-mouse-1] 'imenu-list-display-entry))

;; Define symbols-outline-smart-toggle as a global function
(defun symbols-outline-smart-toggle ()
  "Toggle `symbols-outline-mode' by showing or quitting the `*Outline*' buffer."
  (interactive)
  (if (get-buffer-window "*Outline*" t)
      (quit-window nil (get-buffer-window "*Outline*"))
    (symbols-outline-show)))

(use-package symbols-outline
  :ensure t
  ;; FIXME icons 目錄結構無法保留帶改進
  ;; :quelpa (symbols-outline :repo "liushihao456/symbols-outline.el"
  ;;                          :fetcher github
  ;;                          :files ("*.el" "icons/*"))
  :init
  (setq symbols-outline-window-position 'right
        symbols-outline-collapse-functions-on-startup t
	symbols-outline-fetch-fn #'symbols-outline-lsp-fetch
	symbols-outline-window-width 60
	)
  :hook ((symbols-outline-mode . (lambda () (display-line-numbers-mode -1))))
  :config
  (evil-define-key 'normal symbols-outline-mode-map
    (kbd "r") 'symbols-outline-refresh
    (kbd "q") 'quit-window
    (kbd "j") 'symbols-outline-next
    (kbd "k") 'symbols-outline-prev
    (kbd "d") 'symbols-outline-next-same-level
    (kbd "b") 'symbols-outline-prev-same-level
    (kbd "u") 'symbols-outline-move-depth-up
    (kbd "f") 'symbols-outline-move-depth-down
    (kbd "TAB") 'symbols-outline-toggle-node
    [tab] 'symbols-outline-toggle-node
    (kbd "S-TAB") 'symbols-outline-cycle-visibility-globally
    [backtab] 'symbols-outline-cycle-visibility-globally
    (kbd "RET") 'symbols-outline-visit
    (kbd "M-RET") 'symbols-outline-visit-and-quit)
  (symbols-outline-follow-mode))

(use-package hungry-delete
  :ensure t
  :hook (after-init . global-hungry-delete-mode)
  :config
  (setq-default hungry-delete-chars-to-skip " \t\f\v"))

(use-package expand-region
  :ensure t
  :init
  (setq expand-region-contract-fast-key "V"
	expand-region-reset-fast-key "r"))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :hook (after-init . editorconfig-mode))

;; ;; https://wakatime.com/emacs
;; ;; TODO custom modify: find `wakatime-call', change `"%s--entity %s --plugin \"%s/%s\" --time %.2f%s%s"' to `"'%s--entity %s --plugin \"%s/%s\" --time %.2f%s%s'"'
;; (use-package wakatime-mode
;;   :ensure nil
;;   :quelpa
;;   (wakatime-mode :repo "wakatime/wakatime-mode"
;; 		 :fetcher github
;; 		 :upgrade t)
;;   :diminish 'wakatime-mode
;;   :preface
;;   (defun wakatime-dashboard ()
;;     (interactive)
;;     (browse-url "https://wakatime.com/dashboard"))
;;   :hook
;;   (after-init . global-wakatime-mode)
;;   :config
;;   ;; use `pip install wakatime' and `which wakatime' to get cli path
;;   (setq wakatime-cli-path "~/.wakatime/wakatime-cli")
;;   )

(use-package rg
  :ensure t
  :hook (after-init . rg-enable-default-bindings)
  :config
  (setq rg-group-result t)
  (setq rg-show-columns t)
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)
  (with-eval-after-load 'projectile
    (defalias 'projectile-ripgrep 'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map)))

(use-package avy
  :ensure t
  :defer nil
  :init
  (setq avy-timeout-seconds 0.0))

;; (use-package centered-cursor-mode :ensure t)
(use-package carbon-now-sh :ensure t)
(use-package restart-emacs :ensure t)
(use-package daemons :ensure t)		; system services/daemons

(use-package apheleia
  :ensure t
  :bind ("C-c f" . apheleia-format-buffer)
  :hook (emacs-startup . apheleia-global-mode)
  :config
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(yapf ruff)) ;先用yapf格式化，然後用ruff加工
  (setf (alist-get 'python-mode apheleia-mode-alist) '(isort ruff))
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'vue-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'web-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'prettier apheleia-formatters) '("prettier" "--tab-width" "4" "--bracket-same-line" "true" filepath)) ;modify prettier params
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

(use-package quickrun
  :ensure t
  :commands (quickrun)
  :init
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec . ("%c -std=c++1z %o -o %e %s"
		"%e %a"))
      (:remove . ("%e")))
    :default "c++")
  (quickrun-add-command "python"
    '((:command . "python3"))
    :default "python")
  :config
  (define-key quickrun--mode-map (kbd "C-c C-k") 'quickrun--kill-running-process)
  )
(global-set-key (kbd "<C-return>") 'quickrun)

(use-package docstr
  :hook ((python-mode python-ts-mode) . (lambda ()
                                          ;; (setq-local docstr-desc-summary "")
                                          (docstr-mode 1)))
  :init
  (setq docstr-python-modes '(python-mode python-ts-mode)
        ;; docstr-python-style 'google
        ;; docstr-python-style 'pep-257
        docstr-python-style 'numpy
        docstr-key-support t)
  :config
  ;; config python-ts-mode
  (add-to-list 'docstr-writers-alist '(python-ts-mode . docstr-writers-python))
  (add-to-list 'docstr-prefix-alist '(python-ts-mode . docstr-python-prefix))
  (add-to-list 'docstr-key-sharp-doc-modes 'python-ts-mode)
  (global-docstr-mode)
  (defcustom docstr-python-style 'pep-257
    "Style specification for document string in Python."
    :type '(choice (const :tag "No specify" nil)
                   (const :tag "PEP 257 convention" pep-257)
                   (const :tag "Google style" google)
                   (const :tag "NumPy Style" numpy))
    :group 'docstr)
  )

(provide 'init-tool)
(message "init-tool loaded in '%.2f' seconds" (get-time-diff time-marked))
;;; init-tool.el ends here
