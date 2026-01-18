;;; init-claude.el ---                                  -*- lexical-binding: t; -*-

(mark-time-here)

(use-package vterm
  :ensure t)

(defun pl/claude-project-root ()
  (when-let ((proj (project-current nil)))
    (car (project-roots proj))))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)

  :bind (("C-c C-'" . claude-code-ide-menu)
         ("C-c C-a" . claude-code-ide)
         ("C-c C-s" . claude-code-ide-check-status))

  :config
  (add-to-list 'display-buffer-alist
               '("\\*claude-code\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 100)))

  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-window-side 'right
        claude-code-ide-window-width 100)

  ;; 关键加固 1：确保 claude 在项目根目录启动（行为更接近你手动 vterm cd 后运行）
  (setq claude-code-ide-default-directory-fn #'pl/claude-project-root)

  ;; 关键加固 2：显式把关键 env 固定到 Emacs 进程（避免子进程/daemon 环境不一致）
  ;; 注意：不写 token 到 Emacs 配置里；只保证 base url/timeout 不会丢。
  (setenv "ANTHROPIC_BASE_URL" "https://ark.cn-beijing.volces.com/api/compatible")
  (setenv "API_TIMEOUT_MS" "300000")

  (setq claude-code-ide-system-prompt
        (string-join
         '("你是我的结对程序员。"
           "先给不超过5步的计划，再动手改代码。"
           "改动尽量小；优先给验证命令或测试。"
           "输出必须包含可运行的验证命令。"
           "涉及大范围重构/删除/改名，必须先给diff并拆步执行。")
         "\n"))

  (claude-code-ide-emacs-tools-setup))

(provide 'init-claude)
(message "init-claude loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-claude.el ends here
