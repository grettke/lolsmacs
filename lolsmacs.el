;;; lolsmacs.el --- Law Of Least Surprise Lattice  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Grant Rettke <grant@wisdomandwonder.com>

;; Author: Grant Rettke <grant@wisdomandonder.com>
;; Keywords: convenience, files, frames
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3"))
;; Homepage: https://github.com/grettke/lolsmacs

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

;; Intuitive impersonal settings complying with the Law Of Least Surprise
;; meant for inclusion with any initialization file especially useful to
;; first-time Emacs users or experienced Emacs users looking for focused
;; high-value content to copy.

;;; Code:

(defun lolsmacs-this-must-before-everything-else-in-your-init-file ()
  "When both the source file and its compiled bytecode are present load the source file first.

It would be safer to add this line of code to the first file and loads in your
initialization sequence. However it is probably good enough if you call this
function before anything else."
  (interactive)
  (setq load-prefer-newer t))

(defun lolsmacs-customize ()
  "Store customizations in a separate file."
  (interactive)
  (setq custom-file "custom.el")
  (load custom-file :noerror))

(defun lolsmacs-require-packages ()
  "Load packages used by this package."
  (interactive)
  (require 'whitespace)
  (require 'eldoc))

(defun lolsmacs-persistence ()
  "Remember what you were doing and how things looked and restore it upon restarting."
  (interactive)
  (save-place-mode t)

  (setq savehist-save-minibuffer-history t)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring
          last-kbd-macro
          kmacro-ring
          shell-command-history))
  (savehist-mode)

  (setq desktop-restore-eager 5)
  (desktop-save-mode)

  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (auto-save-visited-mode)

  (global-auto-revert-mode))

(defvar lolsmacs-save-on-hooks
  (append
   '(focus-out-hook mouse-leave-buffer-hook suspend-hook))
  "When they run save all file buffers.")
(defvar
  lolsmacs-save-buffer-only-ons
  (append
   '(vc-diff vc-next-action vc-revert))
  "Before they run save the buffer.")
(defvar
  lolsmacs-save-buffers-ons
  (append
   '(dired eshell grep ibuffer shell)
   '(compile ns-do-hide-emacs execute-extended-command find-file occur goto-line)
   '(eval-buffer)
   '(org-export-dispatch org-babel-tangle org-babel-detangle)
   '(kill-current-buffer list-buffers save-buffers-kill-terminal switch-to-buffer pop-to-buffer)
   '(delete-frame delete-other-frames other-frame suspend-frame)
   '(delete-window quit-window other-window select-window))
  "Before they run save all file buffers.")
(defvar lolsmacs-save-bufs-debug nil "When non-nil message debug information for `lolsmacs-save-bufs'.")
(defun lolsmacs-save-bufs ()
  "Save all file buffers.

When `lolsmacs-save-bufs-debug' is non-nil display performance information in
*Messages* buffer."
  (interactive)
  (let ((time (current-time)))
    (save-some-buffers t nil)
    (when lolsmacs-save-bufs-debug
      (message "lolsmacs-save-bufs completed in: %.06f seconds" (float-time (time-since time))))))
(defun lolsmacs-save-buffers-ons-advice (&rest _args)
  "Helper function for advising `lolsmacs-save-buffers-ons' (for advice ignore _ARGS)."
  (lolsmacs-save-bufs))
(defun lolsmacs-save-buffers-ons-advice-add (fn)
  "Add save on advice to FN."
  (advice-add fn :before #'lolsmacs-save-buffers-ons-advice))
(defun lolsmacs-save-buffer-only-ons-advice (&rest _args)
  "Helper function for advising `lolsmacs-save-buffer-only-ons' (for advice ignore _ARGS)."
  (basic-save-buffer))
(defun lolsmacs-save-buffer-only-ons-advice-add (fn)
  "Add save on advice to FN."
  (advice-add fn :before #'lolsmacs-save-buffer-only-ons-advice))
(defun lolsmacs-persistence-files ()
  "File-related persistence."
  (interactive)
  (mapc (lambda (hook)
          (add-hook hook #'lolsmacs-save-bufs))
        lolsmacs-save-on-hooks)

  (mapc (lambda (fn)
          (lolsmacs-save-buffer-only-ons-advice-add fn))
        lolsmacs-save-buffer-only-ons)

  (mapc (lambda (fn)
          (lolsmacs-save-buffers-ons-advice-add fn))
        lolsmacs-save-buffers-ons)

  (define-key special-event-map [sigusr1] #'lolsmacs-save-bufs))

(defun lolsmacs-display ()
  "Editor appearance."
  (interactive)
  (setq echo-keystrokes 0.02)

  (global-font-lock-mode)

  (setq-default indicate-buffer-boundaries 'left)

  (show-paren-mode)
  (setq show-paren-delay 0)
  (setq show-paren-style 'expression)

  (global-hl-line-mode)

  (setq whitespace-style '(tab-mark))
  (setf
   (cdr (assoc 'tab-mark whitespace-display-mappings))
   '(?\t [?↹ ?\t] [?\t]))
  (global-whitespace-mode)

  (size-indication-mode)

  (column-number-mode)
  (setq column-number-indicator-zero-based nil)

  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (global-prettify-symbols-mode))

(defun lolsmacs-buffers ()
  "Buffer behavior."
  (interactive)
  (defconst lolsmacs-column-width 78)

  (minibuffer-electric-default-mode)

  (electric-pair-mode)

  (delete-selection-mode)

  (setq save-interprogram-paste-before-kill t)

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")

  (setq scroll-preserve-screen-position t)

  (setq scroll-conservatively 101)

  (setq make-pointer-invisible t)

  (setq mouse-drag-copy-region t)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse t)

  (setq track-eol t)

  (setq line-move-visual nil)

  (setq ring-bell-function 'ignore)
  (setq visible-bell t))

(defun lolsmacs-operations ()
  "Editor operations."
  (interactive)
  (setq minibuffer-eldef-shorten-default t)

  (setq resize-mini-windows t)

  (setq max-mini-window-height 0.33)

  (setq history-delete-duplicates t)

  (setq register-preview-delay 2)
  (setq register-separator "\n\n")

  (setq initial-major-mode 'emacs-lisp-mode)
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))

  (setq-default eval-expression-print-level nil)

  (put #'upcase-region 'disabled nil)
  (put #'downcase-region 'disabled nil)

  (setq large-file-warning-threshold (* 1024 1024))

  (setq help-window-select t)

  (setq search-default-mode #'char-fold-to-regexp)

  (setq kill-read-only-ok t)

  (setq comint-scroll-to-bottom-on-input 'this)
  (setq comint-scroll-to-bottom-on-output 'others)
  (setq comint-move-point-for-output 'others)
  (setq comint-scroll-show-maximum-output t)
  (setq comint-move-point-for-output t)
  (setq comint-prompt-read-only nil))

(defun lolsmacs-editing ()
  "Editing things."
  (interactive)
  (setq inhibit-eol-conversion t)

  (setq require-final-newline t)

  (setq-default tab-width 2)

  (delete-selection-mode t)

  (setq-default fill-column lolsmacs-column-width)

  (setq sentence-end-double-space nil)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil))

(defun lolsmacs-init ()
  "Load entire LOLSMacs configuration."
  (interactive)
  (lolsmacs-this-must-before-everything-else-in-your-init-file)
  (lolsmacs-customize)
  (lolsmacs-require-packages)
  (lolsmacs-persistence)
  (lolsmacs-persistence-files)
  (lolsmacs-display)
  (lolsmacs-buffers)
  (lolsmacs-editing))

(provide 'lolsmacs)
;;; lolsmacs.el ends here

