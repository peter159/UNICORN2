;;; init-lsp-python.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  linyi

;; Author: linyi <linyi@ubu-born-0>
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

(defun unicorn/pyenv-executable-find (command)
  "Find executable taking pyenv shims into account.
If the executable is a system executable and not in the same path
as the pyenv version then also return nil. This works around https://github.com/pyenv/pyenv-which-ext
"
  (if (executable-find "pyenv")
      (progn
        (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command)))
              (pyenv-version-names (split-string (string-trim (shell-command-to-string "pyenv version-name")) ":"))
              (executable nil)
              (i 0))
          (if (not (string-match "not found" pyenv-string))
              (while (and (not executable)
                          (< i (length pyenv-version-names)))
                (if (string-match (elt pyenv-version-names i) (string-trim pyenv-string))
                    (setq executable (string-trim pyenv-string)))
                (if (string-match (elt pyenv-version-names i) "system")
                    (setq executable (string-trim (executable-find command))))
                (setq i (1+ i))))
          executable))
    (executable-find command)))

(defun unicorn/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "%s %s"
                                 (unicorn/pyenv-executable-find python-shell-interpreter)
                                 (shell-quote-argument (file-name-nondirectory buffer-file-name)))))
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode)
	))))

(defun unicorn/python-highlight-breakpoint ()
  "highlight a break point"
  (interactive)
  (highlight-lines-matching-regexp "^[ ]*__import__(\"ipdb\").set_trace()" 'hi-salmon)
  (highlight-lines-matching-regexp "^[ ]*import ipdb" 'hi-salmon)
  (highlight-lines-matching-regexp "^[ ]*ipdb.set_trace()" 'hi-salmon))

(defun unicorn/python-insert-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  ;; ref: https://github.com/psf/black/issues/790
  (let ((trace  "__import__(\"ipdb\").set_trace()  # FIXME BREAKPOINT")
	(line (thing-at-point 'line)))
    (if (and line (string-match trace line))
	(kill-whole-line)
      (progn
	(back-to-indentation)
	(insert trace)
	(insert "\n")
	(python-indent-line)
	(unicorn/python-highlight-breakpoint)))))

(defun unicorn/python-delete-breakpoint ()
  "delete break point"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^[ ]*__import__(\"ipdb\").set_trace()")
    (flush-lines "^[ ]*import ipdb[ ]*ipdb.set_trace()")
    (flush-lines "^[ ]*import ipdb")
    (flush-lines "^[ ]*ipdb.set_trace()")))

(defun unicorn/pyright-setup-project-config()
  "setup config env for project root"
  (interactive)
  (shell-command "cp -r ~/.emacs.d/config_snippets/python/.opencode .")
  (shell-command "cp ~/.emacs.d/config_snippets/python/pyrightconfig.json .")
  (shell-command "touch .projectile")
  ;; (shell-command "touch .env")
  )

(defun unicorn/pyright-delete-project-config()
  "delete project config file"
  (interactive)
  (shell-command "rm pyrightconfig.json")
  (shell-command "rm .projectile")
  ;; (shell-command "rm .env")
  )

(defun unicorn/insert_packaging_skeleton()
  "setup config env for project root"
  (interactive)
  (shell-command "mkdir -p src")
  (shell-command "mkdir -p csrc")
  (shell-command "cp ~/.emacs.d/config_snippets/python/cythonize_package.py ./csrc/")
  (shell-command "cp ~/.emacs.d/config_snippets/python/LICENSE .")
  (shell-command "cp ~/.emacs.d/config_snippets/python/pyproject.toml .")
  (shell-command "cp ~/.emacs.d/config_snippets/python/MANIFEST.in .")
  (shell-command "cp ~/.emacs.d/config_snippets/python/setup.cfg .")
  (shell-command "cp ~/.emacs.d/config_snippets/python/wheelbuilder.sh .")
  )

(defun unicorn/pyright-createstub (pkg)
  (interactive "sStub package (import name): ")
  (let* ((root  (expand-file-name "~/.stubs/"))
         (pyi   (expand-file-name "pyright"))
         (name  (string-trim pkg))
         (tdir  (expand-file-name "typings" root))
         (dest  (expand-file-name name tdir)))
    (unless (file-exists-p root) (make-directory root t))
    (unless (file-exists-p tdir) (make-directory tdir t))
    (when (and (file-directory-p dest)
               (yes-or-no-p (format "Stub %s 已存在，删除并重新生成？ " dest)))
      (delete-directory dest t))
    (let ((default-directory root))
      (with-current-buffer (get-buffer-create "*Pyright Stub*") (erase-buffer))
      (let ((code (call-process pyi nil "*Pyright Stub*" t "--createstub" name)))
        (if (eq code 0)
            (message "stub created: %s -> %s/" name dest)
          (error "pyright failed (%s). See *Pyright Stub*" code))))))

(use-package python
  :ensure nil
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (setq python-indent-guess-indent-offset-verbose nil)
  :hook
  (((python-mode python-ts-mode) . (lambda ()
				     (setq-local flycheck-checkers '(python-pylint))
				     (pyvenv-tracking-mode 1) ;autoupdate python venv when switching proj
				     (pyvenv-mode 1)))
   (inferior-python-mode . (lambda ()
			     (process-query-on-exit-flag
			      (get-process "Python")))))
  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python"))
  (define-key python-ts-mode-map (kbd "M--") #'(lambda()(interactive)(insert " -> ")))
  (define-key python-mode-map (kbd "M--") #'(lambda()(interactive)(insert " -> ")))
  (define-key inferior-python-mode-map (kbd "C-n") 'comint-next-input)
  (define-key inferior-python-mode-map (kbd "<up>") 'comint-next-input)
  (define-key inferior-python-mode-map (kbd "C-p") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "<down>") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "<C-k>") 'comint-kill-input)
  (define-key inferior-python-mode-map (kbd "C-r") 'comint-history-isearch-backward)
  (define-key python-mode-map (kbd "C-c C-b") 'unicorn/python-execute-file)
  (define-key python-ts-mode-map (kbd "C-c C-b") 'unicorn/python-execute-file))

;; (use-package py-isort :ensure t)

(use-package pyvenv
  :ensure t
  :preface
  ;; autoload virtual environment if project_root/pyrightconfig.json file exists
  ;; use unicorn/pyright-setup-project-config to init the skeleton
  (defun pyvenv-autoload ()
    (require 'projectile)
    (require 'json)
    (let* ((pdir (projectile-project-root))
           (pfile (concat pdir "pyrightconfig.json"))
           (json-object-type 'hash-table)
           (json-array-type 'string)
           (json-key-type 'string)
           venv-name)
      (if (file-exists-p pfile)
          (progn
            (setq venv-name (gethash "venv" (json-read-file pfile)))
            (make-local-variable 'venv-name)
            (pyvenv-workon venv-name))
        )))
  :hook ((python-mode python-ts-mode) . pyvenv-autoload))

;; 让 Eglot 的 CAPF 永远排在首位（比 yasnippet-capf 更早）
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (cons #'eglot-completion-at-point
                                (remove #'eglot-completion-at-point
                                        completion-at-point-functions))))))

(provide 'init-lsp-python)
(message "init-python loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-lsp-python.el ends here
