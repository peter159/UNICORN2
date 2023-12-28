;;; init-evil.el ---                                 -*- lexical-binding: t; -*-

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

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "C-S-j") 'move-text-down)
(global-set-key (kbd "C-S-k") 'move-text-up)

(use-package evil-leader
  :ensure t
  :defer nil
  :init
  (global-evil-leader-mode))

;; (message "    ---- 1/12 of init-evil loaded using '%.2f' seconds ..." (get-time-diff time-marked))
;; (mark-time-here)

(use-package evil-major-leader
  :ensure nil
  :quelpa
  (evil-major-leader
   :repo "peter159/evil-major-leader"
   :fetcher github)
  :init
  (global-evil-major-leader-mode))

;; (message "    ---- 1/6 of init-evil loaded using '%.2f' seconds ..." (get-time-diff time-marked))
;; (mark-time-here)

(use-package evil
  :ensure t
  :init (setq evil-want-integration t
	      evil-want-keybinding nil)
  :config
  (use-package evil-anzu :ensure t)
  (require 'evil-anzu)
  (evil-mode)
  (progn
    (defun unicorn//evil-visual-shift-left ()
      "evil left shift without losing selection"
      (interactive)
      (call-interactively 'evil-shift-left)
      (evil-normal-state)
      (evil-visual-restore))
    (defun unicorn//evil-visual-shift-right ()
      "evil right shift without losing selection"
      (interactive)
      (call-interactively 'evil-shift-right)
      (evil-normal-state)
      (evil-visual-restore))
    ;; Overload shifts so that they don't lose the selection
    (define-key evil-visual-state-map (kbd "<") 'unicorn//evil-visual-shift-left)
    (define-key evil-visual-state-map (kbd ">") 'unicorn//evil-visual-shift-right)
    (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
    (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
    (define-key evil-insert-state-map (kbd "C-y") 'yank)
    (define-key evil-normal-state-map (kbd "C-a") 'move-beginning-of-line)
    (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
    (define-key evil-normal-state-map (kbd "C-r") 'undo)
    (define-key evil-visual-state-map (kbd "C-a") 'move-beginning-of-line)
    (define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)
    (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
    (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
    (define-key evil-insert-state-map (kbd "C-n") 'next-line)
    (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
    (define-key evil-visual-state-map (kbd "C-S-l") 'enlarge-window)
    (define-key evil-normal-state-map (kbd "C-S-l") 'enlarge-window)
    (define-key evil-insert-state-map (kbd "C-S-l") 'enlarge-window)
    (define-key evil-normal-state-map (kbd ">") 'evil-indent)
    (define-key evil-normal-state-map (kbd "gd") 'xref-find-definitions)
    (define-key evil-normal-state-map (kbd "gD") 'xref-find-definitions-other-window)
    (define-key evil-normal-state-map (kbd "gr") 'xref-find-references)
    )
  )

;; use 'fd' to escape nearly everything from evil-mode
(use-package evil-escape
  :ensure t
  :init
  (setq-default evil-escape-delay 0.3)
  (evil-escape-mode))

;; comment/uncomment, use 'gc' to see how it works
(use-package evil-commentary
  :ensure t
  :hook
  (after-init . evil-commentary-mode))

;; surrounding text shortcut, see: https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode))

(use-package evil-visualstar
  :ensure t
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (progn
    (define-key evil-visual-state-map (kbd "*") 'evil-visualstar/begin-search-forward)
    (define-key evil-visual-state-map (kbd "#") 'evil-visualstar/begin-search-backward)
    ))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; using outline-minor-mode for evil folding
(use-package outline-mode
  :ensure nil
  :hook (prog-mode . outline-minor-mode)
  :init
  ;; (evil-define-key 'normal outline-mode-map (kbd "zK") 'outline-show-branches) ; Show all children recursively but no body.
  ;; (evil-define-key 'normal outline-mode-map (kbd "zk") 'outline-show-children) ; Direct children only unlike `outline-show-branches'
  (define-key evil-normal-state-map (kbd "zB") 'outline-hide-body)
  (define-key evil-normal-state-map (kbd "zb") 'outline-show-all)
  (define-key evil-normal-state-map (kbd "ze") 'outline-show-entry)
  (define-key evil-normal-state-map (kbd "zl") 'outline-hide-leaves)
  (define-key evil-normal-state-map (kbd "zo") 'outline-hide-other)
  (define-key evil-normal-state-map (kbd "zj") 'outline-forward-same-level)
  (define-key evil-normal-state-map (kbd "zk") 'outline-backward-same-level)
  (define-key evil-normal-state-map (kbd "M-j") 'outline-move-subtree-down)
  (define-key evil-normal-state-map (kbd "M-k") 'outline-move-subtree-up))

(use-package dashboard
  :ensure t
  :diminish (dashboard-mode page-break-lines-mode)
  :commands evil-normal-state-map
  :init
  (setq
   dashboard-icon-type 'nerd-icons
   dashboard-banner-logo-title "[U N I C O R N]"
   dashboard-startup-banner (expand-file-name "imgs/unicorn-face.png" user-emacs-directory) ;unicorn.png
   dashboard-center-content nil
   dashboard-items '((recents . 10)
		     (bookmarks . 5)
		     (agenda . 5))
   dashboard-set-file-icons t
   dashboard-set-heading-icons t
   dashboard-heading-icons '((recents   . "nf-oct-history")
			     (bookmarks . "nf-oct-bookmark")
			     (agenda    . "nf-oct-calendar")
			     (projects  . "nf-oct-rocket")
			     (registers . "nf-oct-database"))
   dashboard-set-footer t
   dashboard-footer-icon ""
   dashboard-footer-messages '(" The philosophy of immersive programming"))
  :config
  (evil-define-key 'normal dashboard-mode-map
    (kbd "f") 'widget-button-press)
  (evil-define-key 'normal dashboard-mode-map
    (kbd "g r") 'dashboard-refresh-buffer)
  :hook
  (after-init . dashboard-setup-startup-hook)
  :custom-face
  (dashboard-banner-logo-title ((t (:height 1.1 :inherit default))))
  )

(provide 'init-evil)
(message "init-evil loaded in '%.2f' seconds ..." (get-time-diff time-marked))
;;; init-evil.el ends here
