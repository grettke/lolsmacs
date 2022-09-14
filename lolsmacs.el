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
  '(
    focus-out-hook
    kill-emacs-hook
    mouse-leave-buffer-hook
    suspend-hook
    )
  "Add hook `lolsmacs-save-bufs' to these hooks.")

(defvar
  lolsmacs-save-buffer-only-ons
  '(
    tex-compile
    vc-diff
    vc-ediff
    vc-next-action
    vc-revert
    )
  "Call `lolsmacs-save-buffer-only-ons-advice' BEFORE executing these functions.")

(defvar
  lolsmacs-save-buffers-ons
  '(
    byte-compile-file
    compile
    delete-frame
    delete-other-frames
    delete-window
    dired
    eshell
    eval-buffer
    goto-line
    grep
    ibuffer
    kill-current-buffer
    list-buffers
    ns-do-hide-emacs
    org-babel-detangle
    org-babel-execute-buffer
    org-babel-execute-src-block
    org-babel-execute-subtree
    org-babel-tangle
    org-edit-src-code
    org-export-dispatch
    other-frame
    other-window
    pop-to-buffer
    quit-window
    save-buffers-kill-emacs
    save-buffers-kill-terminal
    select-window
    shell
    suspend-frame
    switch-to-buffer
    tex-compile
    )
  "Call `lolsmacs-save-buffers-ons-advice' BEFORE executing these functions.")

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
  "Delegates work to `lolsmacs-save-bufs'.

Helper function for advising `lolsmacs-save-buffers-ons' (for advice ignore _ARGS)."
  (lolsmacs-save-bufs))
(defun lolsmacs-save-buffers-ons-advice-add (fn)
  "Add save on advice to FN."
  (advice-add fn :before #'lolsmacs-save-buffers-ons-advice))
(defun lolsmacs-save-buffer-only-ons-advice (&rest _args)
  "Save the current buffer.

Helper function for advising `lolsmacs-save-buffer-only-ons' (for advice ignore _ARGS)."
  (basic-save-buffer))
(defun lolsmacs-save-buffer-only-ons-advice-add (fn)
  "Add save on advice to FN."
  (advice-add fn :before #'lolsmacs-save-buffer-only-ons-advice))
(defun lolsmacs-persistence-files ()
  "Granular file-related persistence.

A single value pervades this set up: all development is performed
using file-based artifacts that are as current as possible and
stored in version control. Its motivated by broken builds and
other bizarre conditions due to files being out of sync between
the file system and the editor. The entire persistence set up
deals with this. This function deals with the granular file
management not covered by existing modes. With that in mind here
is where it begins.

We must consider the elephant in the living room: given
`auto-save-visited-mode' is enabled why is this additional
granularity even considered? It is considered because sometimes
`auto-save-visited-mode' (ASVM) isn't fast enough.

For example imagine editing a Makefile in Emacs, switching to a
console terminal (either hosted within Emacs or externally using a
terminal client), hitting the up arrow, and finally returning to execute
Make. You've performed this operation thousands of times and you
do it in milliseconds. Its even faster if you rigged up a macro
to execute an external command to do it. Is ASVM failing you
here? Nope. ASVM is working perfectly well and as expected right
now.

Here is why: ASVM saves file buffers when you've been idle for
`auto-save-visited-interval' seconds. If you make it too large then you
can lose your work because it waited too long. If you make it too
small then it will waste energy and kill performance. ASVM's default settings are
perfect for 99% of its use cases. Once in a while though you need to perform
the same thing a lot sooner than before `auto-save-visited-interval' seconds. The
best way to consider these cases is splitting them up into three broad groups.

There are three frames of mind to get into your cognitive workspace when you
want to configure granular file persistence

  1. Handling Special Events: Events, Hooks, and Keys
  2. Handling Unrelated File Buffers
  3. Handling Related File Buffers

When do you want to automatically save file buffers? For most of us it is most
of the time and ASVM handles that. There are exceptions though when the save
needs to be performed as quickly as possible. Here is the breakdown and
examples of and how it needs to happen.

1. Handling Special Events: Events, Hooks, and Keys

This section tries to handle quitting Emacs in unhappy
unplanned unpleasant ways. Upon failure Emacs will go away on
gracefully, begrudgingly, and by dying. Usually Emacs closes on
the request of `save-buffers-kill-terminal'. Other times it
might be locked up and you send it a signal `(elisp)Event Examples'.
Other times you must kill `(elisp)Killing Emacs' Emacs' it.
This function sets up the 3 ways to handle them:

  A. Before advice for functions
  B. Hooks
  C. Key bindings

It seems to cover most of the worst cases.

2. Handling Unrelated File Buffers

Here is the best example:

The VC package `(emacs)Version Control' `vc-next-action' operates
on a single file `(emacs) Basic VC Editing'. If you make a change
before calling `vc-next-action' VC will ask you if you want to
save your changes before performing the action. Most of us /want/
the changes saved before we perform the next action which is
usually and `add' or `commit' operating to the VC backend. In
fact the intent and expectation of the function is that it will
only ever operate on one file: it is safe to expect that. It is
frustrating being prompted `yes-or-no-p' for something you'll
answer yes to nearly every time. Before-advice saves you from
this pain. There is a good case where this is the wrong
functionality though.

Perhaps you want to be able to perform `vc-next-action' against
the state of the file on the disk, not in the buffer because you
*know* that it is correct. For example if you have an automated
build system that watches for file changes. You made some
changes, saved them, the build system saw them, buiilt them,
and ran all of the unit tests and passed. At the same time, you
notice something in your code and want to add a TODO item.
However you don't want it to be part of the commit. Right now
you have a file on disk that you know is correct and ready to
commit, and changes in your buffer that you don't want to commit.
In this case you want to commit the file without saving the changes.
You need to manage all of this before `auto-save-visited-interval'
and it is realistic to do so.

3. Handling Related File Buffers

Here is an example:

You've got multiple buffers open working on a single project's source code. For
the build to work correctly all of the files need to be persisted to the disk.
As you work on code and move between buffers you need *all* of the files to be
properly persisted (it is the same for auto-build or manual build setups).
There are three ways to address this: 1. Add `lolsmacs-save-buffers-ons' to
`other-window'. 2. Add the same advice to `shell'. 3. Add a hook that calls
`lolsmacs-save-bufs' to `focus-out-hook'. This configuration addresses the
most common development cycle for file based development. However not all
of the  development process is file based.

Some development environments and development cycles aren't
designed strictly around changes being persisted and working off
of a file. One good example is that of TeX. When you perform a
compilation, \"Run TeX on...\"), a TeX file if the compiler runs
into problems it will stop and prompt you what it should do next.
Suppose you got here by running TeX on a file, it ran into a
problem, and now you want to resolve it. When you use `tex-mode'
you have two ways of running TeX on a file: the functions
`tex-file' or `tex-buffer'. When `tex-offer-save' is non-nil the
former asks if you want to save all file based buffers then runs
TeX. The latter takes the contents of the current buffer, saves
them to a temporary file, and runs TeX over it. The formers seems to be
simpler and more predictable even if you are just playing around with what you
might do next but it is a good example of when you might not want all of your
files to be persisted as quickly as possible. "
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
  (setq show-paren-style 'mixed)

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

  (defconst lolsmacs-column-width 80)

  (minibuffer-electric-default-mode)

  (electric-pair-mode)

  (delete-selection-mode 1)

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

  ;; Here is the scenario for this style of Comint configuration:
  ;;
  ;; You are doing a lot of interactive work via various Comint-supported
  ;; buffers. You are working in one buffer (the one with focus) while the
  ;; others are doing their own thing. They are probably doing work and output
  ;; is scrolling by and that is fine because you are not reading it. In the
  ;; buffer you are working in though, you want to go back and read something.
  ;; So although it its process continues to output information, you want to
  ;; keep the cursor in the same spot. Then when you are ready to type a
  ;; command (suppose you know the output has stopped) to do something else,
  ;; when you type the cursor will go to the end of the buffer. That is why
  ;; you prevent the focused buffer from auto-scrolling and moving the mark,
  ;; and leave the other ones alone.
  (setq comint-scroll-to-bottom-on-input 'this)
  (setq comint-scroll-to-bottom-on-output 'others)
  (setq comint-move-point-for-output 'others)
  (setq comint-scroll-show-maximum-output t)
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

