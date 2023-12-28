;;; init-font.el ---                                 -*- lexical-binding: t; -*-

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

(setq-local unicorn-font-size 11)
;; setup english word font and size
(set-face-attribute 'default nil :font (format "JetBrains Mono-%S" unicorn-font-size)) ; Fira Code Retina-%S; Fira Code Retina-%S
(set-face-attribute 'fixed-pitch-serif nil :family "Iosevka")
;; (set-face-attribute 'default nil :font (format "Fira Code Retina-%S" 12)) ; Fira Code Retina-%S; Fira Code Retina-%S
(setq-default line-spacing 0.2)

;; fix the delay when showing text in chinese
(dolist (charset '(kana han cjk-misc bopomofo))
  (if (display-graphic-p)		;to avoid error 'fontset tty' in linux shell environment
      (set-fontset-font (frame-parameter nil 'font) charset
			;; (font-spec :family "Microsoft Yahei" :size 12))
			(font-spec :family "等距更纱黑体 SC" :size unicorn-font-size)))
  )

;; (use-package fontify-face :ensure t)

;; sudo apt-get install librime-dev
(defun unicorn/candidate-show-framework ()
  (if (display-graphic-p)
      'posframe
    'popup))

;; add rime support
(use-package rime
  :ensure t
  :quelpa
  (rime :fetcher github
        :repo "DogLooksGood/emacs-rime"
        :files ("*.el" "Makefile" "lib.c"))
  :init
  (setq default-input-method "rime"
        rime-user-data-dir (expand-file-name "data/rime" user-emacs-directory)
        rime-show-candidate (unicorn/candidate-show-framework)
        rime-posframe-properties (list :internal-border-width 1))
  :bind
  (("C-S-i" . toggle-input-method)
   ))


(provide 'init-font)
(message "init-font loaded in '%.2f' seconds ..." (get-time-diff time-marked))
