;;; init-golang.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  linyi

;; Author: linyi <linyi@ubun-born-0>
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
;; ref :https://dreamerjonson.com/2019/11/24/emacs-package-for-golang/index.html
;; 

;;; Code:

(mark-time-here)

;; Golang
(use-package go-mode
  :ensure t
  :functions go-update-tools
  :commands godoc-gogetdoc
  :init (setq godoc-at-point-function #'godoc-gogetdoc)
  :hook (before-save . gofmt-before-save)
  :custom (gofmt-command "goimports")
  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

  ;; Install or update tools
  (defvar go--tools '("golang.org/x/tools/gopls"
                      "golang.org/x/tools/cmd/goimports"
                      "honnef.co/go/tools/cmd/staticcheck"
                      "github.com/go-delve/delve/cmd/dlv"
                      "github.com/zmb3/gogetdoc"
                      "github.com/josharian/impl"
                      "github.com/cweill/gotests/..."
                      "github.com/fatih/gomodifytags"
                      "github.com/davidrjenni/reftools/cmd/fillstruct")
    "All necessary go tools.")

  (defun go-update-tools ()
    "Install or update go tools."
    (interactive)
    (unless (executable-find "go")
      (user-error "Unable to find `go' in `exec-path'!"))

    (message "Installing go tools...")
    (dolist (pkg go--tools)
      (set-process-sentinel
       (start-process "go-tools" "*Go Tools*" "go" "install" "-v" "-x" (concat pkg "@latest"))
       (lambda (proc _)
         (let ((status (process-exit-status proc)))
           (if (= 0 status)
               (message "Installed %s" pkg)
             (message "Failed to install %s: %d" pkg status)))))))

  ;; Try to install go tools if `gopls' is not found
  (unless (executable-find "gopls")
    (go-update-tools)))

;; Misc
(use-package go-dlv :ensure t)
(use-package go-fill-struct :ensure t)
(use-package go-impl :ensure t)

;; Install: See https://github.com/golangci/golangci-lint#install
(use-package flycheck-golangci-lint
  :ensure t
  :if (executable-find "golangci-lint")
  :after flycheck
  :defines flycheck-disabled-checkers
  :hook (go-mode . (lambda ()
                     "Enable golangci-lint."
                     (setq flycheck-disabled-checkers '(go-gofmt
                                                        go-golint
                                                        go-vet
                                                        go-build
                                                        go-test
                                                        go-errcheck))
                     (flycheck-golangci-lint-setup))))

(use-package go-tag
  :ensure t
  :init (setq go-tag-args (list "-transform" "camelcase")))

(use-package go-gen-test :ensure t)
(use-package gotest :ensure t)

(provide 'init-lsp-golang)
(message "init-lsp-golang loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-golang.el ends here
