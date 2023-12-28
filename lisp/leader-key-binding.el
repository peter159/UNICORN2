;;; leader-key-binding.el ---                        -*- lexical-binding: t; -*-

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

(defvar unicorn-evil-leader-key "<SPC>"
  "Evil leader key.")

(evil-leader/set-leader "<SPC>")
(evil-major-leader/set-leader "\,")

(evil-leader/set-key
  ;; "'"   'unicorn/pop-eshell
  "'"   'my-shell-here
  ;; "?"   'counsel-descbinds
  "/"   'consult-ripgrep
  "v"   'er/expand-region
  "u"   'universal-argument
  "Ts"  'consult-theme
  "TF"  'toggle-frame-fullscreen
  "TM"  'maximize-window
  "TAB" 'unicorn/alternate-buffer
  "d"   'xref-go-back)

;; leader-a application family
(which-key-add-key-based-replacements
  (format "%s a" unicorn-evil-leader-key) "application")
(which-key-add-key-based-replacements
  (format "%s ao" unicorn-evil-leader-key) "org")
(which-key-add-key-based-replacements
  (format "%s aC" unicorn-evil-leader-key) "clock")
(evil-leader/set-key
  "ad"  'deer
  "ap"  'list-processes
  "aP"  'proced
  "ar"  'ranger
  "ak"  'package-list-packages	     ;'paradox-list-packages
  "ao#" 'org-agenda-list-stuck-projects
  "ao/" 'org-occur-in-agenda-files
  "aoa" 'org-agenda-list
  "aoc" 'org-capture
  "aoe" 'org-store-agenda-views
  "aofi" 'org-feed-goto-inbox
  "aofu" 'org-feed-update-all
  "aoCc" 'org-clock-cancel
  "aoCg" 'org-clock-goto
  "aoCi" 'org-clock-in
  "aoCI" 'org-clock-in-last
  "aoCj" 'org-clock-jump-to-current-clock
  "aoCo" 'org-clock-out
  "aoCr" 'org-resolve-clocks
  "aol" 'org-store-link
  "aom" 'org-tags-view
  "aoo" 'org-agenda
  "aos" 'org-search-view
  "aot" 'org-todo-list)

;; leader-q family
(which-key-add-key-based-replacements
  (format "%s q" unicorn-evil-leader-key) "quit")
(evil-leader/set-key
  "qq" 'unicorn/frame-killer
  "qQ" 'kill-emacs
  "qh" 'suspend-frame
  "qR" 'restart-emacs)

;; leader-h family
(which-key-add-key-based-replacements
  (format "%s h" unicorn-evil-leader-key) "helps")
(evil-leader/set-key
  "hdf" 'describe-function
  "hdv" 'describe-variable)

;; leader-f family
(which-key-add-key-based-replacements
  (format "%s f" unicorn-evil-leader-key) "files")
(which-key-add-key-based-replacements
  (format "%s fy" unicorn-evil-leader-key) "copy")
(which-key-add-key-based-replacements
  (format "%s fv" unicorn-evil-leader-key) "variable")
(evil-leader/set-key
  "ff"  'find-file
  "fj"  'dired-jump
  "ft"  'treemacs
  "fB"  'treemacs-bookmark
  "fT"  'treemacs-find-file
  "fL"  'consult-locate
  "fr"  'consult-recent-file
  "fR"  'unicorn/rename-current-buffer-file
  "fs"  'save-buffer
  "fS"  'evil-write-all
  "fc"  'unicorn/copy-file
  "fi"  'insert-file
  "fb"  'consult-bookmark
  "fB"  'treemacs-bookmark
  "fvd" 'add-dir-local-variable
  "fvf" 'add-file-local-variable
  "fvp" 'add-file-local-variable-prop-line
  "fCu" 'dos2unix
  "fCd" 'unix2dos
  "fyy" 'unicorn/copy-file-path
  "fyY" 'unicorn/projectile-copy-file-path
  "fyd" 'unicorn/copy-directory-path
  "fyn" 'unicorn/copy-file-name
  "fec" 'unicorn/find-custom-file
  "fed" 'unicorn/find-dotfile)

;; leader-g family
(which-key-add-key-based-replacements
  (format "%s g" unicorn-evil-leader-key) "git")
(evil-leader/set-key
  "gc"  'magit-clone
  "gff" 'magit-find-file
  "gfl" 'magit-log-buffer-file
  "gfd" 'magit-diff
  "gi"  'magit-init
  "gl"  'magit-log-head
  "gL"  'magit-list-repositories
  "gm"  'magit-dispatch
  "gs"  'magit-status
  "gS"  'magit-stage-file
  "gU"  'magit-unstage-file
  "gho" 'browse-at-remote)

;; leader-i insert family
(which-key-add-key-based-replacements
  (format "%s i" unicorn-evil-leader-key) "insert")
(evil-leader/set-key
  "is" 'ivy-yasnippet)

;; leader-p family
(which-key-add-key-based-replacements
  (format "%s p" unicorn-evil-leader-key) "project")
(evil-leader/set-key
  "p SPC" 'consult-projectile
  "p'"    'unicorn/projectile-pop-eshell
  "pt"    'unicorn/treemacs-project-toggle
  "pb"    'consult-projectile-switch-to-buffer
  "pd"    'consult-projectile-find-dir
  "pp"    'consult-projectile-switch-project
  "pf"    'consult-projectile-find-file
  "pr"    'projectile-recentf
  "po"    'org-projectile/goto-todos
  "pl"    'unicorn/ivy-persp-switch-project
  "pv"    'projectile-vc)

;; leader-j family
(which-key-add-key-based-replacements
  (format "%s j" unicorn-evil-leader-key) "jumps")
(evil-leader/set-key
  "ji" 'unicorn/consult-jump-in-buffer
  "jw" 'evil-avy-goto-word-or-subword-1
  "jD" 'deer-jump-other-window
  "jd" 'deer
  "jj" 'avy-goto-char-timer
  "jJ" 'avy-goto-char-2)

;; leader-e family
(which-key-add-key-based-replacements
  (format "%s e" unicorn-evil-leader-key) "error")
(evil-leader/set-key
  "eb" 'flycheck-buffer
  "ec" 'flycheck-clear
  "eh" 'flycheck-describe-checker
  "el" 'flycheck-list-errors
  "en" 'flycheck-next-error
  "ep" 'flycheck-previous-error
  "es" 'flycheck-select-checker
  "eS" 'flycheck-set-checker-executable
  "ev" 'flycheck-verify-setup
  "ey" 'flycheck-copy-errors-as-kill
  "ex" 'flycheck-explain-error-at-point)

;; leader-b family
(which-key-add-key-based-replacements
  (format "%s b" unicorn-evil-leader-key) "buffers")
(evil-leader/set-key
  "bb" 'consult-buffer
  "bd" 'kill-this-buffer
  "bR" 'unicorn/revert-current-buffer
  "bs" 'unicorn/goto-scratch-buffer
  "bx" 'kill-buffer-and-window
  "bk" 'kill-buffer
  "bh" 'unicorn/goto-dashboard
  "bY" 'unicorn/copy-whole-buffer-to-clipboard
  "ba" 'persp-add-buffer
  "br" 'persp-remove-buffer
  "bi" 'symbol-outline-or-imenu-list-toggle
  ;; "bi" 'lsp-ui-imenu
  "bI" 'ibuffer)

;; leader-t family
(which-key-add-key-based-replacements
  (format "%s t" unicorn-evil-leader-key) "toggle")
(evil-leader/set-key
  "t-" 'centered-cursor-mode
  "ts" 'flycheck-mode)

;; leader-w family
(which-key-add-key-based-replacements
  (format "%s w" unicorn-evil-leader-key) "window")
(evil-leader/set-key
  "wm"  'unicorn/toggle-maximize-buffer
  "wH"  'evil-window-move-far-left
  "wL"  'evil-window-move-far-right
  "wJ"  'evil-window-move-very-bottom
  "wK"  'evil-window-move-very-top
  "wh"  'evil-window-left
  "wl"  'evil-window-right
  "wj"  'evil-window-down
  "wk"  'evil-window-up
  "wv"  'evil-window-vsplit
  "ws"  'evil-window-split
  "wc"  'olivetti-mode
  "wd"  'delete-window
  "wF"  'make-frame
  "wpm" 'popwin:messages
  "wpp" 'popwin:close-popup-window
  "wb"  'wakatime-dashboard)

;; leader-o family
(which-key-add-key-based-replacements
  (format "%s o" unicorn-evil-leader-key) "yours")
(evil-leader/set-key
  "ow"  'whitespace-cleanup
  "obs" 'bookmark-set
  "obd" 'bookmark-delete
  "obr" 'bookmark-rename
  "obl" 'bookmark-bmenu-list
  "oh" 'browse-url)

;;;; major mode specific keybinding
(which-key-add-key-based-replacements
  (format "%s m" unicorn-evil-leader-key) "major mode cmds")

(defun unicorn//set-key-prefix-name (key name)
  (which-key-add-key-based-replacements (format "%s m%s" unicorn-evil-leader-key key) name)
  (which-key-add-key-based-replacements (format ", %s" key) name))

(unicorn//set-key-prefix-name "c" "compile")
(unicorn//set-key-prefix-name "d" "debug")
(unicorn//set-key-prefix-name "g" "goto")
(unicorn//set-key-prefix-name "s" "REPL")
(unicorn//set-key-prefix-name "h" "help")
(unicorn//set-key-prefix-name "v" "virtualenv")

;;;; major mode specific keybinding
(unicorn//setup-default-key-name "m" "major mode cmds")
(unicorn//setup-major-mode-key-name "=" "format")
(unicorn//setup-major-mode-key-name "b" "backend")
(unicorn//setup-major-mode-key-name "c" "compile")
(unicorn//setup-major-mode-key-name "d" "debug")
(unicorn//setup-major-mode-key-name "g" "goto")
(unicorn//setup-major-mode-key-name "G" "goto other window")
(unicorn//setup-major-mode-key-name "p" "peek")
(unicorn//setup-major-mode-key-name "F" "folders")
(unicorn//setup-major-mode-key-name "r" "refactor")
(unicorn//setup-major-mode-key-name "rg" "generate")
(unicorn//setup-major-mode-key-name "ra" "assign/add")
(unicorn//setup-major-mode-key-name "s" "REPL")
(unicorn//setup-major-mode-key-name "t" "test")
(unicorn//setup-major-mode-key-name "T" "toggles")
(unicorn//setup-major-mode-key-name "h" "help")
(unicorn//setup-major-mode-key-name "v" "virtualenv")
(unicorn//setup-major-mode-key-name "vp" "pipenv")

;;; lsp major mode settings
(dolist (mode unicorn-lsp-active-modes)
  (evil-leader/set-key-for-mode mode
    ;; format
    "m=b" #'lsp-format-buffer
    "m=r" #'lsp-format-region
    ;; goto
    "mgr" #'lsp-find-references
    "mgt" #'lsp-find-type-definition
    "mgd" #'lsp-find-definition
    "mgD" #'lsp-find-declaration
    "mgi" #'lsp-find-implementation
    "mgk" #'unicorn/lsp-avy-goto-word
    "mgK" #'unicorn/lsp-avy-goto-symbol
    "mgM" #'lsp-ui-imenu
    ;; goto other window
    "mGr" #'unicorn/lsp-find-references-other-window
    "mGt" #'unicorn/lsp-find-type-definition-other-window
    "mGd" #'unicorn/lsp-find-definition-other-window
    "mGD" #'unicorn/lsp-find-declaration-other-window
    "mGi" #'unicorn/lsp-find-implementation-other-window
    ;; peek
    "mpd" #'lsp-ui-peek-find-definitions
    "mpi" #'lsp-ui-peek-find-implementation
    "mpr" #'lsp-ui-peek-find-references
    "mpRn" #'lsp-ui-find-next-reference
    "mpRp" #'lsp-ui-find-prev-reference
    ;; backend
    "mba" #'lsp-execute-code-action
    "mbd" #'lsp-describe-session
    "mbr" #'lsp-restart-workspace
    "mbs" #'lsp-shutdown-workspace
    ;; refactor
    "mrr" #'lsp-rename
    ;; toggles
    "mTd" #'lsp-ui-doc-mode
    "mTs" #'lsp-ui-sideline-mode
    "mTF" #'unicorn/lsp-ui-doc-func
    "mTS" #'unicorn/lsp-ui-sideline-symb
    "mTI" #'unicorn/lsp-ui-sideline-ignore-duplicate
    "mTl" #'lsp-lens-mode
    ;; folders
    "mFs" #'lsp-workspace-folders-switch
    "mFr" #'lsp-workspace-folders-remove
    "mFa" #'lsp-workspace-folders-add)
  )

;;; json
(evil-leader/set-key-for-mode 'json-mode "m=" 'prettier-js)

;;; python
(evil-leader/set-key-for-mode 'python-ts-mode "m=" 'yapfify-buffer)
(evil-leader/set-key-for-mode 'python-ts-mode "msb" 'python-shell-send-buffer)
(evil-leader/set-key-for-mode 'python-ts-mode "mck" 'unicorn/quit-subjob)
(evil-leader/set-key-for-mode 'python-ts-mode "mdb" 'unicorn/python-insert-breakpoint)
(evil-leader/set-key-for-mode 'python-ts-mode "mdd" 'unicorn/python-delete-breakpoint)
(evil-leader/set-key-for-mode 'python-ts-mode "mdh" 'unicorn/python-highlight-breakpoint)
(evil-leader/set-key-for-mode 'python-ts-mode "msk" 'unicorn/python-interrupt-repl)
(evil-leader/set-key-for-mode 'python-ts-mode "msq" 'unicorn/python-quit-repl)
(evil-leader/set-key-for-mode 'python-ts-mode "msr" 'python-shell-send-region)
(evil-leader/set-key-for-mode 'python-ts-mode "mva" 'pyvenv-activate)
(evil-leader/set-key-for-mode 'python-ts-mode "mvd" 'pyvenv-deactivate)
(evil-leader/set-key-for-mode 'python-ts-mode "mvw" 'pyvenv-workon)

(if (eq system-type 'windows-nt)
    (progn
      (evil-leader/set-key-for-mode 'python-ts-mode "mcc" 'unicorn/windows-python-execute-file)
      (evil-leader/set-key-for-mode 'python-ts-mode "mcC" 'unicorn/windows-python-execute-file-focus)
      (evil-leader/set-key-for-mode 'python-ts-mode "msi" 'unicorn/windows-python-start-or-switch-repl)
      )
  (progn
    (evil-leader/set-key-for-mode 'python-ts-mode "mcc" 'unicorn/python-execute-file)
    (evil-leader/set-key-for-mode 'python-ts-mode "mcC" 'unicorn/python-execute-file-focus)
    (evil-leader/set-key-for-mode 'python-ts-mode "msi" 'unicorn/python-start-or-switch-repl)
    ))

(provide 'leader-key-binding)
(message "leader-key-binding loaded in '%.2f' seconds" (get-time-diff time-marked))
;;; leader-key-binding.el ends here
