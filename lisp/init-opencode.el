;;; init-opencode.el ---                                 -*- lexical-binding: t; -*-

(when (fboundp 'mark-time-here)
  (mark-time-here))

(defgroup pl-opencode nil
  "OpenCode integration."
  :group 'tools)

(defcustom pl/opencode-window-side 'right
  "Where to display the OpenCode vterm window."
  :type '(choice (const left) (const right) (const bottom) (const top))
  :group 'pl-opencode)

(defcustom pl/opencode-window-width 100
  "Window width (or height if side is top/bottom) for OpenCode."
  :type 'integer
  :group 'pl-opencode)

(defcustom pl/opencode-command "opencode"
  "Command to start OpenCode in vterm."
  :type 'string
  :group 'pl-opencode)

(defcustom pl/opencode-start-timeout 3.0
  "Seconds to wait for vterm process to become ready."
  :type 'number
  :group 'pl-opencode)

(defcustom pl/opencode-system-prompt nil
  "System prompt text to paste into OpenCode. Nil means using default prompt."
  :type '(choice (const :tag "Default" nil) string)
  :group 'pl-opencode)

(defun pl/opencode--default-system-prompt ()
  (mapconcat
   #'identity
   '("你是我的结对程序员。"
     "先给不超过5步的计划，再动手改代码。"
     "改动尽量小；优先给验证命令或测试。"
     "涉及大范围重构/删除/改名，先给diff并拆步执行。")
   "\n"))

(defun pl/opencode--project-root ()
  (when (require 'project nil t)
    (let ((proj (and (fboundp 'project-current) (project-current nil))))
      (when proj
        (car (project-roots proj))))))

(defun pl/opencode--root ()
  (file-name-as-directory
   (expand-file-name (or (pl/opencode--project-root) default-directory))))

(defun pl/opencode--project-slug (root)
  (let* ((dir (directory-file-name (expand-file-name root)))
         (base (file-name-nondirectory dir)))
    (replace-regexp-in-string "[^[:alnum:]_.-]" "_" base)))

(defun pl/opencode--buffer-name (&optional root)
  (let* ((r (or root (pl/opencode--root)))
         (slug (pl/opencode--project-slug r)))
    (format "*opencode:%s*" slug)))

(defun pl/opencode--started-key (root)
  (intern (format "pl/opencode-started:%s" (pl/opencode--project-slug root))))

(defun pl/opencode--ensure-display-rule ()
  (add-to-list
   'display-buffer-alist
   `( "\\*opencode:.*\\*"
      (display-buffer-in-side-window)
      (side . ,pl/opencode-window-side)
      (window-width . ,pl/opencode-window-width))))

(defun pl/opencode--buffer-live-p (&optional root)
  (let* ((r (or root (pl/opencode--root)))
         (buf (get-buffer (pl/opencode--buffer-name r))))
    (when (buffer-live-p buf)
      (if (pl/opencode--proc-live-p buf)
          buf
        ;; buffer还在但进程死了：清理标记，避免“无法重新打开”
        (let ((started-key (pl/opencode--started-key r)))
          (put started-key 'started nil))
        (kill-buffer buf)
        nil))))

(defun pl/opencode--proc-live-p (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (and (boundp 'vterm--process)
           (process-live-p vterm--process)))))

(defun pl/opencode--ui-running-p (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((end (point-max))
             (beg (max (point-min) (- end 4000)))
             (tail (buffer-substring-no-properties beg end)))
        (or (string-match-p "Ask anything" tail)
            (string-match-p "ctrl\\+p commands" tail)
            (string-match-p "tab agents" tail)
            (string-match-p "OpenCode" tail)
            (string-match-p "opencode" tail))))))

(defun pl/opencode--beautify-buffer ()
  (setq-local display-line-numbers nil)
  (setq-local truncate-lines t)
  (setq-local scroll-margin 0)
  (setq-local left-fringe-width 0)
  (setq-local right-fringe-width 0))

(defun pl/opencode--require-vterm ()
  "Ensure vterm is available. Do not load it at init time."
  (unless (fboundp 'vterm)
    (require 'vterm nil t))
  (unless (fboundp 'vterm)
    (user-error "vterm is not available (check init-shell)")))

(defun pl/opencode--start-process-in-vterm ()
  (vterm-send-string (format "exec %s" pl/opencode-command))
  (vterm-send-return))

(defun pl/opencode--open-vterm (&optional root)
  (pl/opencode--ensure-display-rule)
  (let* ((r (or root (pl/opencode--root)))
         (default-directory r)
         (bufname (pl/opencode--buffer-name r))
         (buf (pl/opencode--buffer-live-p r)))
    (if buf
        (progn
          (display-buffer buf)
          buf)
      (pl/opencode--require-vterm)
      (setq buf (vterm bufname))
      (display-buffer buf)
      (with-current-buffer buf
        (when (boundp 'vterm--process)
          (set-process-sentinel
           vterm--process
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let ((b (process-buffer proc)))
                 (when (buffer-live-p b)
                   (let ((win (get-buffer-window b)))
                     (when (window-live-p win)
                       (delete-window win)))
                   (kill-buffer b))))))))
      buf)))

(defun pl/opencode--wait-ready (buf)
  (let* ((deadline (+ (float-time) pl/opencode-start-timeout))
         proc)
    (with-current-buffer buf
      (while (and (< (float-time) deadline)
                  (not (and (boundp 'vterm--process)
                            (setq proc vterm--process)
                            (process-live-p proc))))
        (accept-process-output nil 0.05)))
    (and proc (process-live-p proc))))

(defun pl/opencode--select-window (buf)
  (let ((win (get-buffer-window buf)))
    (when (window-live-p win)
      (select-window win))))

(defun pl/opencode-open ()
  "Open OpenCode vterm buffer for current project and start opencode once."
  (interactive)
  (let* ((root (pl/opencode--root))
         (buf (pl/opencode--open-vterm root))
         (started-key (pl/opencode--started-key root)))
    (unless (pl/opencode--proc-live-p buf)
      (let ((started-key (pl/opencode--started-key root)))
	(put started-key 'started nil))
      (when (buffer-live-p buf)
	(kill-buffer buf))
      (setq buf (pl/opencode--open-vterm root)))
    (with-current-buffer buf
      (pl/opencode--select-window buf)
      (pl/opencode--beautify-buffer)
      (let ((need-start (or (not (get started-key 'started))
			    (not (pl/opencode--ui-running-p buf)))))
	(when need-start
	  (setq-local header-line-format "Starting OpenCode…")
	  (unless (pl/opencode--wait-ready buf)
	    (setq-local header-line-format nil)
	    (user-error "Vterm process not ready (timeout %.1fs)" pl/opencode-start-timeout))
	  (pl/opencode--start-process-in-vterm)
	  (run-with-timer
	   0.3 nil
	   (lambda (b)
	     (when (buffer-live-p b)
               (with-current-buffer b
		 (setq-local header-line-format nil))))
	   buf)
	  (put started-key 'started t)))
      )))

(defun pl/opencode-focus ()
  (interactive)
  (pl/opencode-open))

(defun pl/opencode-reset-session ()
  "Kill OpenCode buffer for current project and restart."
  (interactive)
  (let* ((root (pl/opencode--root))
         (buf (pl/opencode--buffer-live-p root))
         (started-key (pl/opencode--started-key root)))
    (when buf (kill-buffer buf))
    (put started-key 'started nil)
    (pl/opencode-open)))

(defun pl/opencode-kill ()
  "Kill OpenCode buffer for current project."
  (interactive)
  (let* ((root (pl/opencode--root))
         (buf (pl/opencode--buffer-live-p root))
         (started-key (pl/opencode--started-key root)))
    (when buf
      (kill-buffer buf)
      (put started-key 'started nil)
      (message "OpenCode buffer killed: %s" (pl/opencode--buffer-name root)))))

(defun pl/opencode-paste-system-prompt ()
  "Paste system prompt into OpenCode vterm."
  (interactive)
  (let* ((root (pl/opencode--root))
         (buf (pl/opencode--open-vterm root))
         (prompt (or pl/opencode-system-prompt (pl/opencode--default-system-prompt))))
    (with-current-buffer buf
      (pl/opencode--select-window buf)
      (pl/opencode--beautify-buffer)
      (unless (pl/opencode--wait-ready buf)
        (user-error "Vterm process not ready (timeout %.1fs)" pl/opencode-start-timeout))
      (vterm-send-string prompt)
      (vterm-send-return))))

(defun pl/opencode-send-region (beg end)
  "Send region text to OpenCode vterm."
  (interactive "r")
  (let* ((root (pl/opencode--root))
         (text (buffer-substring-no-properties beg end))
         (buf (pl/opencode--open-vterm root)))
    (with-current-buffer buf
      (pl/opencode--select-window buf)
      (pl/opencode--beautify-buffer)
      (unless (pl/opencode--wait-ready buf)
        (user-error "Vterm process not ready (timeout %.1fs)" pl/opencode-start-timeout))
      (vterm-send-string text)
      )))

(defun pl/opencode-send-buffer ()
  "Send whole buffer to OpenCode vterm."
  (interactive)
  (pl/opencode-send-region (point-min) (point-max)))

(defun pl/opencode-auth-status ()
  "Show `opencode auth status` output."
  (interactive)
  (let* ((default-directory (pl/opencode--root))
         (cmd (concat pl/opencode-command " auth status 2>&1"))
         (out (shell-command-to-string cmd)))
    (message "%s" out)))

(defun pl/opencode-menu ()
  "A small transient-like menu (no leader keys involved)."
  (interactive)
  (let ((key (read-key
              (concat
               "[OpenCode] "
               "o:open  f:focus  P:prompt  r:region  b:buffer  s:status  R:reset  k:kill  q:quit"))))
    (pcase key
      (?o (pl/opencode-open))
      (?f (pl/opencode-focus))
      (?P (pl/opencode-paste-system-prompt))
      (?r (call-interactively #'pl/opencode-send-region))
      (?b (pl/opencode-send-buffer))
      (?s (pl/opencode-auth-status))
      (?R (pl/opencode-reset-session))
      (?k (pl/opencode-kill))
      (_ (message "OpenCode: quit")))))

(provide 'init-opencode)

(when (and (fboundp 'get-time-diff) (boundp 'time-marked))
  (message "init-opencode loaded in '%.2f' seconds ..." (get-time-diff time-marked)))

;;; init-opencode.el ends here
