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

;; download from https://github.com/blahgeek/emacs-lsp-booster and install emacs-lsp-booster binary to `/usr/bin/`
(use-package eglot-booster
  :ensure nil
  :init
  (setq eglot-booster-no-remote-boost t)
  :after eglot
  :quelpa
  (eglot-booster
   :repo "jdtsmith/eglot-booster"
   :fetcher github
   :files ("*.el"))
  :config (eglot-booster-mode))

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
  ((python-mode python-ts-mode) . (lambda()
				    (eglot-booster-mode t)
				    (eglot-ensure)))
  ((ess-r-mode) . eglot-ensure)
  )

(use-package pretty-hydra
  :ensure t
  :init
  (require 'pretty-hydra)
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:inherit highlight :reverse-video t)))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))

(use-package dape
  :ensure t
  :bind (("<f5>" . dape)
         ("M-<f5>" . dape-hydra/body))
  :custom (dape-buffer-window-arrangment 'right)
  :pretty-hydra
  ((:title (pretty-hydra-title "Debug" 'codicon "nf-cod-debug")
	   :color pink :quit-key ("q" "C-g"))
   ("Stepping"
    (("n" dape-next "next")
     ("s" dape-step-in "step in")
     ("o" dape-step-out "step out")
     ("c" dape-continue "continue")
     ("p" dape-pause "pause")
     ("k" dape-kill "kill")
     ("r" dape-restart "restart")
     ("D" dape-disconnect-quit "disconnect"))
    "Switch"
    (("m" dape-read-memory "memory")
     ("t" dape-select-thread "thread")
     ("w" dape-watch-dwim "watch")
     ("S" dape-select-stack "stack")
     ("i" dape-info "info")
     ("R" dape-repl "repl"))
    "Breakpoints"
    (("b" dape-breakpoint-toggle "toggle")
     ("l" dape-breakpoint-log "log")
     ("e" dape-breakpoint-expression "expression")
     ("B" dape-breakpoint-remove-all "clear"))
    "Debug"
    (("d" dape "dape")
     ("Q" dape-quit "quit" :exit t))))
  :init
  (setq dape-cwd-fn 'projectile-project-root)
  :config
  (setq dape-buffer-window-arrangement 'right)
  (plist-put (alist-get 'debugpy dape-configs) 'command "python3")

  ;; ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks
  ;;           (defun dape--save-on-start ()
  ;;             (save-some-buffers t t)))
  ;; ;; Display hydra on startup
  ;; (add-hook 'dape-on-start-hooks #'dape-hydra/body)
  )

(provide 'init-eglot)
(message "init-eglot loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-lsp.el ends here
