;; -*- lexical-binding: t -*-
;;; early-init.el --- Emacs Initialization File

;; Filename: early-init.el
;; Description: my emacs configuration
;; Package-Requires: ((emacs "26.1"))
;; Author: Hiroyuki Deguchi <deguchi@ai.cs.ehime-u.ac.jp>
;; Created: 2018-05-26
;; Modified: 2021-02-04
;; Version: 0.0.4
;; Keywords: internal, local
;; Human-Keywords: Emacs Initialization
;; Namespace: my:
;; URL: https://github.com/de9uch1/emacs.d

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;; Code:

;;; Startup
;;;; Tuning and Speed Up:
;; Disable magic file name at initialize
(defconst my:saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq file-name-handler-alist my:saved-file-name-handler-alist)))
;; Avoid garbage collection in initialize
(setq gc-cons-percentage 1.0
      gc-cons-threshold most-positive-fixnum
      read-process-output-max (* 64 1024 1024))
(add-hook
 'after-init-hook
 `(lambda ()
    (setq gc-cons-threshold (* 128 1024 1024)
          gc-cons-percentage 0.6
          read-process-output-max (* 16 1024 1024))) t)
(run-with-idle-timer 60.0 t #'garbage-collect)
;;; Appearance
;;;; Disable noisy effects
(push '(menu-bar-lines     . nil) default-frame-alist)
(push '(tool-bar-lines     . nil) default-frame-alist)
(push '(scroll-bar-mode    . nil) default-frame-alist)
(push '(column-number-mode . nil) default-frame-alist)
(push '(cursor-type        . bar) default-frame-alist)
(set-scroll-bar-mode nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq frame-inhibit-implied-resize t)
(setq site-run-file nil)
(setq package-enable-at-startup nil)
;;;; for GUI
;; window size
(push '(height . 72) default-frame-alist)
(push '(width . 144) default-frame-alist)
(setq initial-frame-alist default-frame-alist)

(provide 'early-init)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8-unix
;; mode: emacs-lisp
;; mode: outline-minor
;; time-stamp-pattern: "10/Modified:\\\\?[ \t]+%:y-%02m-%02d\\\\?\n"
;; End:

;;; init.el ends here
