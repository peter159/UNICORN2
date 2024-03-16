;;; init-flycheck.el ---                             -*- lexical-binding: t; -*-

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

(use-package flymake
  :ensure nil
  :diminish
  :functions my-elisp-flymake-byte-compile
  :hook (prog-mode . flymake-mode)
  :init
  (evil-define-key 'normal flymake-diagnostics-buffer-mode-map (kbd "RET") 'flymake-goto-diagnostic)
  (evil-define-key 'normal flymake-diagnostics-buffer-mode-map (kbd "j") 'flymake-goto-next-error)
  (evil-define-key 'normal flymake-diagnostics-buffer-mode-map (kbd "k") 'flymake-goto-prev-error)
  (setq flymake-fringe-indicator-position 'right-fringe
        flymake-no-changes-timeout nil
        flymake-start-on-save-buffer t
        )
  :config
  ;; Check elisp with `load-path'
  (defun my-elisp-flymake-byte-compile (fn &rest args)
    "Wrapper for `elisp-flymake-byte-compile'."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around #'my-elisp-flymake-byte-compile))

(use-package sideline-flymake
  :ensure nil
  :quelpa
  (sideline-flymake
   :repo "emacs-sideline/sideline-flymake"
   :fetcher github)
  :diminish sideline-mode
  :hook (flymake-mode . sideline-mode)
  :init (setq sideline-flymake-display-mode 'point
              sideline-backends-right '(sideline-flymake)))

(use-package flymake-ruff
  :ensure nil
  :quelpa
  (flymake-ruff
   :repo "erickgnavar/flymake-ruff"
   :fetcher github)
  :demand t
  :preface
  (defun unicorn/setup-flymake-ruff ()
    (setq-local lsp-diagnostics-provider :none)
    (flymake-ruff-load))
  (defun unicorn/eglot-setup-flymake-ruff ()
    (interactive)
    (when (memq major-mode '(python-mode python-ts-mode))
      (flymake-ruff-load)))
  :config
  (add-hook 'eglot-managed-mode-hook 'unicorn/eglot-setup-flymake-ruff)
  (defun my-filter-eglot-diagnostics (diags)
    "Drop all Pyright diagnose from langserver"
    (list (seq-remove (lambda (d)
                        (string-match "Pyright" (flymake-diagnostic-text d)))
                      (car diags))))
  (advice-add 'eglot--report-to-flymake :filter-args #'my-filter-eglot-diagnostics))

(provide 'init-flymake)
(message "init-flymake loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-flymake.el ends here
