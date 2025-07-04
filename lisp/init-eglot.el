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
  :config
  (setq eglot-send-changes-idle-time 0.2
	eglot-autoshutdown t
	eglot-connect-timeout 1200
	eglot-ignored-server-capabilities '(
					    ;; :inlayHintProvider
					    :documentHighlightProvider
					    :documentonTypeFormattingProvider
					    )
	eldoc-echo-area-use-multiline-p t
	eglot-server-programs '(
				((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))
				((ess-r-mode) . ("R" "--slave" "-e" "languageserver::run()"))
				((c++-mode c-mode c++-ts-mode c-ts-mode objc-mode) . ("clangd"
										      ;; 在后台自动分析文件（基于complie_commands)
										      "--background-index"
										      ;; 标记compelie_commands.json文件的目录位置
										      "--compile-commands-dir=build"
										      ;; 全局补全（会自动补充头文件）
										      "--all-scopes-completion"
										      ;; 更详细的补全内容
										      "--completion-style=detailed"
										      ;; 同时开启的任务数量
										      "-j=12"
										      "-cross-file-rename"
										      ;;clang-tidy功能
										      "--clang-tidy"
										      "--clang-tidy-checks=performance-*,bugprone-*"
										      ;; 告诉clangd用那个clang进行编译，路径参考which clang++的路径
										      ;; "--query-driver=/opt/llvm/bin/clang++"
										      ;; 同时开启的任务数量
										      ;; 补充头文件的形式
										      ;; "--header-insertion=iwyu"
										      ;; pch优化的位置
										      ;; "--pch-storage=disk"
										      ))
				((cmake-mode cmake-ts-mode) . ("cmake-language-server"))
				((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
				((vue-ts-mode) . ("vls" "--stdio")) ; vue2
				((js-ts-mode typescript-ts-mode typescript-mode tsx-ts-mode) . ("typescript-language-server" "--stdio"))
				((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode) . ("gopls"))
				((yaml-ts-mode yaml-mode) . ("yaml-language-server" "--stdio"))
				((dockerfile-mode dockerfile-ts-mode) . ("docker-langserver" "--stdio"))))
  :hook
  ((python-mode python-ts-mode) . (lambda()
				    (eglot-booster-mode t)
				    (eglot-ensure)))
  ((c++-mode c-mode c++-ts-mode c-ts-mode objc-mode) . (lambda()
							 (eglot-booster-mode t)
							 (eglot-ensure)))
  ((ess-r-mode) . eglot-ensure)
  ((js-ts-mode json-ts-mode yaml-ts-mode typescript-ts-mode tsx-ts-mode java-ts-mode mhtml-mode css-ts-mode vue-ts-mode) . eglot-ensure)
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
  )

;; (use-package vue-ts-mode
;;   :ensure nil
;;   :mode ("\\.vue\\'" . vue-ts-mode)
;;   :quelpa
;;   (vue-ts-mode :fetcher github
;; 	       :repo "8uff3r/vue-ts-mode")
;;   :config
;;   ;; from https://github.com/joaotavora/eglot/discussions/1184
;;   (with-eval-after-load 'eglot
;;     (defun vue-eglot-init-options ()
;;       ;; installed with (my/eglot-server--npm-dependancy-install "typescript")
;;       (let ((tsdk-path (expand-file-name
;;                         "lib"
;;                         (string-trim-right
;;                          (shell-command-to-string "npm list --global --parseable typescript | head -n1 | tr -d \"\n\"")))))
;;         `(:typescript (:tsdk ,tsdk-path
;;                              :languageFeatures (:completion
;;                                                 (:defaultTagNameCase "both"
;;                                                                      :defaultAttrNameCase "kebabCase"
;;                                                                      :getDocumentNameCasesRequest nil
;;                                                                      :getDocumentSelectionRequest nil)
;;                                                 :diagnostics
;;                                                 (:getDocumentVersionRequest nil))
;;                              :documentFeatures (:documentFormatting
;;                                                 (:defaultPrintWidth 100
;;                                                                     :getDocumentPrintWidthRequest nil)
;;                                                 :documentSymbol t
;;                                                 :documentColor t)))))
;;     (push `(vue-ts-mode . ("vue-language-server" "--stdio"
;;                            :initializationOptions ,(vue-eglot-init-options)))
;;           eglot-server-programs)))

(provide 'init-eglot)
(message "init-eglot loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-lsp.el ends here
