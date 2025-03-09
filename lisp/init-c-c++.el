;;; init-c-c++.el ---                                -*- lexical-binding: t; -*-

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
;; if need dap mdoe for cpp, refer to here: https://emacs-lsp.github.io/dap-mode/page/configuration/
;; ref: https://tuhdo.github.io/c-ide.html
;; debug usage
;; C-c C-c g++ -g -o test myproj.cpp
;; C-c C-b gdb -i=mi test
;; https://linuxhint.com/c_emacs_configuration/ FIXME config this when available

;; 

;;; Code:

(mark-time-here)

(defun unicorn/clang-setup-project-config()
  "setup config env for project root"
  (interactive)
  (shell-command "cp ~/.emacs.d/config_snippets/cpp/.clangd .")
  )

(defun generate_compile_commands ()
  "检查当前文件所在目录是否为 build 目录，如果不是则拒绝执行；
否则将 ~/.emacs.d/config_snippets/cpp 目录下的 .clangd 和 CMakeLists.txt 文件复制到当前文件所在目录的上一层（如果不存在），
并在当前目录下执行 cmake 命令生成 compile_commands.json 文件。"
  (interactive)
  (let* ((current-dir (file-name-directory (or buffer-file-name default-directory)))
         (dir-name (file-name-nondirectory (directory-file-name current-dir))))
    (if (not (string= dir-name "build"))
        (error "当前文件不在 build 目录下，无法执行 generate_compile_commands")
      (let ((parent-dir (file-name-directory (directory-file-name current-dir))))
        ;; 仅在目标文件不存在时复制文件到上一级目录
        (unless (file-exists-p (expand-file-name ".clangd" parent-dir))
          (copy-file "~/.emacs.d/config_snippets/cpp/.clangd" parent-dir t))
        (unless (file-exists-p (expand-file-name "CMakeLists.txt" parent-dir))
          (copy-file "~/.emacs.d/config_snippets/cpp/CMakeLists.txt" parent-dir t))
        ;; 在当前目录下执行 cmake 命令
        (let ((default-directory current-dir))
          (shell-command "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .."))))))

(use-package cc-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist
	       `("\\.h\\'" . c++-mode))
  :config
  (require 'compile)
  (add-to-list 'auto-mode-alist '("\.cu$" . c++-ts-mode))
  (progn
    (setq-default c-default-style "linux"
		  c-basic-offset 4))
  )

(use-package smart-semicolon
  :ensure t
  :defer t
  :hook (((c-mode-common c++-ts-mode) . smart-semicolon-mode)))

(use-package modern-cpp-font-lock
  :ensure t
  :hook ((c++-mode c++-ts-mode) . modern-c++-font-lock-mode))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
  :config
  (add-hook 'cmake-mode-hook (lambda()
                               (add-to-list (make-local-variable 'company-backends)
                                            'company-cmake)))
  )

(use-package google-c-style
  :ensure t
  :init
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

;; run dap-cpptools-setup to setup automatically
;; (require 'dap-cpptools)

(provide 'init-c-c++)
(message "init-c-c++ loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-c-c++.el ends here
