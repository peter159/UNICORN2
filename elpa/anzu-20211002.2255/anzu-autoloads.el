;;; anzu-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from anzu.el

(autoload 'anzu-mode "anzu" "\
minor-mode which display search information in mode-line.

This is a minor mode.  If called interactively, toggle the `Anzu
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `anzu-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(put 'global-anzu-mode 'globalized-minor-mode t)
(defvar global-anzu-mode nil "\
Non-nil if Global Anzu mode is enabled.
See the `global-anzu-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-anzu-mode'.")
(custom-autoload 'global-anzu-mode "anzu" nil)
(autoload 'global-anzu-mode "anzu" "\
Toggle Anzu mode in all buffers.
With prefix ARG, enable Global Anzu mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Anzu mode is enabled in all buffers where `anzu--turn-on' would do it.

See `anzu-mode' for more information on Anzu mode.

(fn &optional ARG)" t)
(autoload 'anzu-query-replace-at-cursor "anzu" "\
Replace symbol at cursor with to-string." t)
(autoload 'anzu-query-replace-at-cursor-thing "anzu" "\
Replace symbol at cursor within `anzu-replace-at-cursor-thing' area." t)
(autoload 'anzu-query-replace "anzu" "\
anzu version of `query-replace'.

(fn ARG)" t)
(autoload 'anzu-query-replace-regexp "anzu" "\
anzu version of `query-replace-regexp'.

(fn ARG)" t)
(autoload 'anzu-replace-at-cursor-thing "anzu" "\
anzu-query-replace-at-cursor-thing without query." t)
(autoload 'anzu-isearch-query-replace "anzu" "\
anzu version of `isearch-query-replace'.

(fn ARG)" t)
(autoload 'anzu-isearch-query-replace-regexp "anzu" "\
anzu version of `isearch-query-replace-regexp'.

(fn ARG)" t)
(register-definition-prefixes "anzu" '("anzu"))

;;; End of scraped data

(provide 'anzu-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; anzu-autoloads.el ends here
