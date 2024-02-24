;;; init-default.el ---                              -*- lexical-binding: t; -*-

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

(global-set-key (kbd "C-S-f") 'forward-sexp)
(global-set-key (kbd "C-S-b") 'backward-sexp)

(eval-when-compile
  (require 'init-const))

;; ;; use UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; (setq source-directory
;;       (file-name-directory
;;        (shell-command-to-string
;; 	(concat "locate --limit 1 emacs-" emacs-version "/src")))) ;locate emacs source code if exists

(setq source-directory
      (let ((shell-output (shell-command-to-string
                           (concat "locate --limit 1 emacs-" emacs-version "/src"))))
        (if (string-blank-p shell-output)
            "~/software/emacs30_treesitter/src"  ; 指定的路径，当 shell 命令返回为空时使用
          (file-name-directory shell-output))))

(setq delete-by-moving-to-trash t)	; Deleting files go to OS's trash folder
(setq make-backup-files nil)		; Forbide to make backup files
(setq auto-save-default nil)		; Disable auto save
(setq create-lockfiles nil)		; Disable lock files .#filename
(setq truncate-lines t)			; do not truncate long lines, to avoid wrong display

(setq-default major-mode 'text-mode)

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; Line and Column
(setq-default fill-column 120)
(setq column-number-mode t)		; show in mode-line
;; (setq line-number-mode t)

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Basic modes

;; Start server
(use-package server
  :ensure nil
  :config
  (when (eq system-type 'windows-nt)
    (defun server-ensure-safe-dir (dir) "Noop" t) ;stop warning for server owner nil
    )
  (server-start)
  :hook (after-init . server-mode))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Automatically reload file that was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :defines dired-mode-map
  :bind (("C-c C-z ." . browse-url-at-point)
         ("C-c C-z b" . browse-url-of-buffer)
         ("C-c C-z r" . browse-url-of-region)
         ("C-c C-z u" . browse-url)
         ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 100)
  (setq recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "ido.*"
                          "persp-confs"
                          "recentf"
                          "url"
                          "COMMIT_EDITMSG\\'")))

;; ;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
					      global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; ;; Automatic parenthesis pairing
;; (use-package elec-pair
;;   :ensure nil
;;   :hook (after-init . electric-pair-mode)
;;   :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Make bindings that stick around
(use-package hydra :ensure t)

;; (use-package electric-spacing
;;   :ensure t
;;   :hook
;;   (prog-mode . electric-spacing-mode))

(use-package electric-operator
  :ensure t
  :hook
  (prog-mode . electric-operator-mode)
  :config
  (electric-operator-add-rules-for-mode 'emacs-lisp-mode
					(cons "-" "-"))
  (electric-operator-add-rules-for-mode 'lisp-interaction-mode
					(cons "-" "-"))
  )

(use-package undo-tree
  :ensure nil
  :config
  (global-undo-tree-mode nil))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; use exec-path-from-shell in linux/mac
(when (or (eq system-type 'gnu/linux) (eq system-type 'darwin))
  (use-package exec-path-from-shell
    :ensure t
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"
					   "WORKON_HOME" "JAVA_HOME"
					   "LLVM_HOME" "LD_LIBRARY_PATH"
					   "LSP_USE_PLISTS"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; Cross-referencing commands
(use-package xref
  :init
  ;; Use faster search tool
  (setq xref-search-program (cond
                             ((executable-find "ugrep") 'ugrep)
                             ((executable-find "rg") 'ripgrep)
                             (t 'grep)))
  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

(provide 'init-default)
(message "init-default loaded in '%.2f' seconds ..." (get-time-diff time-marked))
