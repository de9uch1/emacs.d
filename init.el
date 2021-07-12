;; -*- lexical-binding: t -*-
;;; init.el --- Emacs Initialization File

;; Filename: init.el
;; Description: my emacs configuration
;; Package-Requires: ((emacs "26.1"))
;; Author: Hiroyuki Deguchi <deguchi.hiroyuki.db0@is.naist.jp>
;; Created: 2018-05-26
;; Modified: 2021-07-12
;; Version: 0.0.3
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
;;;; Profiler
;; (require 'profiler)
;; (profiler-start 'cpu)
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "init time: %.3f sec"
                     (float-time (time-subtract after-init-time before-init-time)))))
;;;; Tuning and Speed Up:
(defconst early-init-compat (version<= "27.1" emacs-version))
(unless early-init-compat
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
  (run-with-idle-timer 60.0 t #'garbage-collect))


;;;; cl-lib -- load Common Lisp library:
;; (eval-when-compile (require 'cl-lib nil t))
(setq byte-compile-warnings '(cl-functions))

;;; System Local
(defvar my:distrib-id nil)
(defvar my:gentoo-p nil)
(eval-when-compile
  (when (file-exists-p "/etc/gentoo-release")
    (setq my:distrib-id "gentoo")
    (setq my:gentoo-p t)))

;;; My Functions and Macros -- prefix "my:"
(defun my:ne (x y &optional comp)
  "Return t if X not eq Y.
COMP is used instead of eq when COMP is given."
  (not
   (if comp
       (funcall comp x y)
     (eq x y))))
(defmacro my:path-exists? (path)
  "Return PATH if PATH exists else nil."
  `(if (file-exists-p ,path)
       ,path
     nil))
(defmacro my:locate-user-emacs-file (x)
  "Expand filename (locate-user-emacs-file X)."
  `(expand-file-name (locate-user-emacs-file ,x)))
;; my:locate-home
(defconst HOME (getenv "HOME"))
(defmacro my:locate-home (x)
  "Concat and expand path X from HOME."
  `(expand-file-name ,x ,HOME))
;; mode enable/disable
(defmacro my:enable-mode (mode)
  "Enable MODE."
  `(,mode 1))
(defmacro my:disable-mode (mode)
  "Disable MODE."
  `(,mode 0))
;; suppressed message
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

;;; My Configurations
;; Name
(setq user-full-name "Hiroyuki Deguchi")
;; E-mail Address
(setq user-mail-address "deguchi.hiroyuki.db0@is.naist.jp")
;;;; Directory --
;; $HOME/.emacs.d
;; for ``emacs -q -l .emacs''
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(defconst my:d:tmp (my:locate-user-emacs-file "tmp"))
(defconst my:d:share (my:locate-user-emacs-file "share"))
;; Nextcloud
(defconst my:d:nextcloud
  (let ((d (my:locate-home "Nextcloud")))
    (if (file-exists-p d)
        d
      user-emacs-directory)))

;;; Package Management
;;;; package.el
(require 'package nil t)
(custom-set-variables
 '(package-archives '(("melpa" . "https://melpa.org/packages/")
                      ("gnu" . "https://elpa.gnu.org/packages/"))))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;;;; use-package.el
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))
(use-package use-package-ensure-system-package
  :ensure t
  :config
  (setq system-packages-package-manager
        (cond (my:gentoo-p 'emerge))))
(use-package diminish
  :ensure t)
(use-package bind-key
  :ensure t)
;;;; quelpa, quelpa-use-package
(use-package quelpa
  :ensure t
  :config
  (setq quelpa-upgrade-p nil
        quelpa-checkout-melpa-p nil
        quelpa-update-melpa-p nil
        quelpa-melpa-recipe-stores nil))
(use-package quelpa-use-package
  :ensure t)

;;; Custom
(setq custom-file (my:locate-user-emacs-file "custom.el"))
;; (load custom-file t)

;;; Theme
;;;; Color Theme
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-neotree-project-size 1.0)
  (doom-themes-neotree-folder-size 1.0)
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))
;;;; Modeline
;; doom-modeline
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-icon t)
  (doom-modeline-project-detection 'projectile)
  :config
  (my:enable-mode doom-modeline-mode)
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info major-mode process vcs checker)))
;; hide-mode-line
(use-package hide-mode-line
  :ensure t
  :hook
  ((neotree-mode imenu-list-minor-mode) . hide-mode-line-mode))
;;;; Font
;; all-the-icons
(use-package all-the-icons
  :ensure t
  :custom
  (all-the-icons-scale-factor 1.0)
  :config
  (use-package all-the-icons-dired
    :ensure t
    :hook (dired-mode . all-the-icons-dired-mode))
  (use-package all-the-icons-ibuffer
    :ensure t
    :hook (after-init . all-the-icons-ibuffer-mode)))
;; icons-in-terminal.el -- for non-GUI
(use-package all-the-icons-in-terminal
  :quelpa (all-the-icons-in-terminal :fetcher github :repo uwabami/isfit-plus :files (:defaults "data"))
  :if (not window-system)
  :after all-the-icons
  ;; :custom
  ;; (icons-in-terminal-scale-factor 1.0)
  ;; :config
  ;; (defalias #'all-the-icons-insert #'icons-in-terminal-insert)
  ;; (defalias #'all-the-icons-insert-faicon #'icons-in-terminal-insert-faicon)
  ;; (defalias #'all-the-icons-insert-fileicon #'icons-in-terminal-insert-fileicon)
  ;; (defalias #'all-the-icons-insert-material #'icons-in-terminal-insert-material)
  ;; (defalias #'all-the-icons-insert-octicon #'icons-in-terminal-insert-octicon)
  ;; (defalias #'all-the-icons-insert-wicon #'icons-in-terminal-insert-wicon)

  ;; ;; (defalias #'all-the-icons-icon-for-dir #'icons-in-terminal-icon-for-dir)
  ;; (defalias #'all-the-icons-icon-for-dir-with-chevron #'icons-in-terminal-icon-for-dir)
  ;; (defalias #'all-the-icons-icon-for-file #'icons-in-terminal-icon-for-file)
  ;; (defalias #'all-the-icons-icon-for-mode #'icons-in-terminal-icon-for-mode)
  ;; (defalias #'all-the-icons-icon-for-url #'icons-in-terminal-icon-for-url)

  ;; (defalias #'all-the-icons-icon-family #'icons-in-terminal-icon-family)
  ;; (defalias #'all-the-icons-icon-family-for-buffer #'icons-in-terminal-icon-family-for-buffer)
  ;; (defalias #'all-the-icons-icon-family-for-file #'icons-in-terminal-icon-family-for-file)
  ;; (defalias #'all-the-icons-icon-family-for-mode #'icons-in-terminal-icon-family-for-mode)
  ;; (defalias #'all-the-icons-icon-for-buffer #'icons-in-terminal-icon-for-buffer)

  ;; (defalias #'all-the-icons-faicon #'icons-in-terminal-faicon)
  ;; (defalias #'all-the-icons-octicon #'icons-in-terminal-octicon)
  ;; (defalias #'all-the-icons-fileicon #'icons-in-terminal-fileicon)
  ;; (defalias #'all-the-icons-material #'icons-in-terminal-material)
  ;; (defalias #'all-the-icons-wicon #'icons-in-terminal-wicon)

  ;; (defalias 'icons-in-terminal-dir-icon-alist 'icons-in-terminal-dir-icon-spec)
  ;; (defalias 'icons-in-terminal-weather-icon-alist 'icons-in-terminal-weather-icon-spec)

  ;; (defalias 'all-the-icons-default-adjust 'icons-in-terminal-default-adjust)
  ;; (defalias 'all-the-icons-color-icons 'icons-in-terminal-color-icons)
  ;; (defalias 'all-the-icons-scale-factor 'icons-in-terminal-scale-factor)
  ;; (defalias 'all-the-icons-icon-alist 'icons-in-terminal-icon-alist)
  ;; (defalias 'all-the-icons-dir-icon-alist 'icons-in-terminal-dir-icon-alist)
  ;; (defalias 'all-the-icons-weather-icon-alist 'icons-in-terminal-weather-icon-alist)
  )
;; Fontset -- Cica: https://github.com/miiton/Cica
(when window-system
  (set-fontset-font "fontset-standard" 'unicode (font-spec :family "Cica" :size 16))
  (use-package all-the-icons
    :config
    (unless (x-list-fonts "all-the-icons")
      (all-the-icons-install-fonts t))
    (set-fontset-font "fontset-standard" 'unicode (font-spec :family (all-the-icons-alltheicon-family)) nil 'append)
    (set-fontset-font "fontset-standard" 'unicode (font-spec :family (all-the-icons-material-family)) nil 'append)
    (set-fontset-font "fontset-standard" 'unicode (font-spec :family (all-the-icons-fileicon-family)) nil 'append)
    (set-fontset-font "fontset-standard" 'unicode (font-spec :family (all-the-icons-faicon-family)) nil 'append)
    (set-fontset-font "fontset-standard" 'unicode (font-spec :family (all-the-icons-octicon-family)) nil 'append)
    (set-fontset-font "fontset-standard" 'unicode (font-spec :family (all-the-icons-wicon-family)) nil 'append))
  (set-face-font 'default "fontset-standard")
  (push '(font . "fontset-standard") default-frame-alist)
  (setq initial-frame-alist default-frame-alist))
;;;; Misc.
(unless early-init-compat
  ;; disable menu-bar, tool-bar, scroll-bar
  (my:disable-mode menu-bar-mode)
  (my:disable-mode tool-bar-mode)
  (set-scroll-bar-mode nil)
  ;; cursor
  (push '(cursor-type . bar) default-frame-alist)
  ;; No startup screen
  (setq inhibit-startup-screen t)
  ;; window size
  (when window-system
    (pcase (system-name)
      (_ (progn (push '(height . 56) default-frame-alist)
                (push '(width . 117) default-frame-alist))))
    (setq initial-frame-alist default-frame-alist)))
;; truncate lines
(setq-default truncate-lines t)
(add-hook 'org-mode-hook #'(lambda () (setq-local truncate-lines nil)))
;; transparent-mode
(use-package tp-mode
  :if window-system
  :quelpa (tp-mode :fetcher github :repo de9uch1/tp-mode)
  :config
  (tp-mode 95))
;; display line number
(if (version<= "26.1" emacs-version)
    (progn
      (global-display-line-numbers-mode)
      (setq-default display-line-numbers-width 4))
  (progn
    (my:enable-mode global-linum-mode)
    (setq linum-format "%5d ")))
;; highlight line
(my:enable-mode global-hl-line-mode)
(add-hook
 'doc-view-mode-hook
 #'(lambda ()
     (progn
       (my:disable-mode linum-mode)
       (my:disable-mode hl-line-mode))))
;; show paren
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;;; Basic Configurations
;;;; Language, Locale and Coding System
(set-locale-environment "ja_JP.UTF-8")
;; (setq-default file-name-coding-system 'utf-8-unix)
(setq-default system-time-locale "C")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
;; (set-selection-coding-system 'utf-8-unix)
;; (set-buffer-file-coding-system 'utf-8-unix)
;; (set-language-environment 'Japanese)
;; (setq-default buffer-file-coding-system 'utf-8-unix)
;;;; Misc.
;; load-path
(let ((default-directory my:d:share))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))
;;  PATH -- exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))
;; Bell
;; Alternative flash the screen
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
;; Indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; key binding
(bind-keys
 ("C-m" . newline-and-indent)
 ("C-h" . delete-backward-char)) ; C-h -> Backspace
;; .el > .elc
(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))
;; scroll for one line
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ; for shell-mode
;; *.~ / .#* no back up
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix (expand-file-name my:d:tmp ".saves-"))
;; definition temporary files and shared files
(setq url-configuration-directory (expand-file-name "url" my:d:tmp)
      nsm-settings-file (expand-file-name "network-settings.data" my:d:tmp)
      bookmark-default-file (expand-file-name "bookmarks" my:d:share))
;; save minibuffer history
(my:enable-mode savehist-mode)
(setq message-log-max 10000)
(setq history-length t
      savehist-file (expand-file-name "history" my:d:tmp))
;; yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; not add newline at end of buffer
(setq next-line-add-newlines nil)
;; kill whole line when kill line
(setq kill-whole-line t)
;; global-auto-revert-mode
(my:enable-mode global-auto-revert-mode)
;; time-stamp
(use-package time-stamp
  :commands time-stamp
  :hook (before-save . #'time-stamp))
;; Emacs Server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))
;; tramp
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-persistency-file-name (expand-file-name "tramp" my:d:tmp)))
;; dired
(use-package dired-aux
  :defer t
  :config
  (use-package dired-async))
(setq dired-dwim-target t               ; default copy target in 2 windows
      dired-recursive-copies 'always    ; recursive directory copy
      dired-isearch-filenames t)        ; only match filenames
;; generic-x
(use-package generic-x)
;; save last opened place
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "save-places" my:d:tmp))
  (my:enable-mode save-place-mode))
;; for same name buffer
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-min-dir-content 1))
;; XClip
(when (eq window-system 'x)
  (setq x-select-enable-clipboard t))
;; popwin
(use-package popwin
  :ensure t
  :hook (after-init . popwin-mode)
  :config
  (push '("*Compile-Log*") popwin:special-display-config)
  (push '("*Buffer List*") popwin:special-display-config)
  (push '("*Warnings*") popwin:special-display-config)
  (push '("*system-packages*") popwin:special-display-config)
  (push '("*Async Shell Command*") popwin:special-display-config))
;; move window
(use-package windmove
  :bind (("M-<left>" . windmove-left)
         ("M-<right>" . windmove-right)
         ("M-<up>" . windmove-up)
         ("M-<down>" . windmove-down)))
;; eldoc
(my:disable-mode global-eldoc-mode)
(use-package eldoc-overlay
  :ensure t
  :config
  (setq eldoc-idle-delay 30))

;;; Global Packages
(use-package evil
  :ensure t
  :custom
  (evil-toggle-key "M-q")
  :config
  (my:disable-mode evil-mode))
;;;; eshell
(use-package eshell
  :bind ("M-e" . eshell)
  :hook (eshell-mode . (lambda () (bind-keys :map eshell-mode-map
                                             ("C-d" . delete-char)
                                             ("C-a" . eshell-bol))))
  :config
  (setq my:d:eshell (expand-file-name "eshell" my:d:tmp))
  ;; lastdir & history
  (setq eshell-last-dir-ring-file-name (expand-file-name (concat "lastdir." (system-name)) my:d:eshell))
  (setq eshell-history-file-name (expand-file-name (concat "history." (system-name)) my:d:eshell))
  ;; misc
  (setq eshell-cmpl-ignore-case t)                 ; 補完時に大文字小文字を区別しない
  (setq eshell-ask-to-save-history (quote always)) ; 確認なしでヒストリ保存
  (setq eshell-cmpl-cycle-completions t)           ; 補完時にサイクルする
  (setq eshell-cmpl-cycle-cutoff-length 5)         ; 補完候補がこの数値以下だとサイクルせずに候補表示
  (setq eshell-hist-ignoredups t)
  ;; set eshell aliases
  (setq eshell-command-aliases-list
	    '(("ll" "ls -lh $*")
          ("la" "ls -a $*")
          ("lla" "ls -lha $*")
          ("findn" "find . -name $*")
          ("duc" "du -had1 $*"))))
;;;; prescient.el -- simple but effective sorting and filtering for Emacs.
(use-package prescient
  :ensure t
  :custom
  (prescient-aggressive-file-save t)
  (prescient-save-file (expand-file-name "prescient-save.el" my:d:tmp))
  (prescient-history-length 5000)
  :config
  (my:enable-mode prescient-persist-mode))
;;;; counsel/ivy, swiper
;; ivy
(use-package ivy
  :ensure t
  :diminish
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 30)
  (ivy-wrap t)
  (ivy-format-functions-alist '((t . ivy-format-function-arrow)))
  (ivy-count-format (concat (all-the-icons-faicon "sort-amount-asc") " (%d/%d) "))
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq enable-recursive-minibuffers t)
  (my:enable-mode ivy-mode)
  ;; ivy-rich
  (use-package ivy-rich
    :ensure t
    :after ivy
    :config
    (my:enable-mode ivy-rich-mode))
  ;; all-the-icons-ivy
  (use-package all-the-icons-ivy
    :ensure t
    :disabled t
    :after all-the-icons
    :config
    (all-the-icons-ivy-setup))
  (use-package all-the-icons-ivy-rich
    :ensure t
    :after (all-the-icons ivy-rich)
    :config
    (all-the-icons-ivy-rich-mode))
  ;; ivy-posframe
  (use-package ivy-posframe
    :ensure t
    :disabled t
    :custom
    (ivy-posframe-display-functions-alist
     '((t . ivy-posframe-display-at-point)))
    :config
    (my:enable-mode ivy-posframe-mode))
  (use-package ivy-prescient
    :ensure t
    :hook (ivy-mode . ivy-prescient-mode)
    :custom
    (ivy-prescient-retain-classic-highlighting t)
    :config
    (setf (alist-get 'swiper ivy-re-builders-alist) #'my:ivy-migemo-re-builder)
    ;; (setf (alist-get t ivy-re-builders-alist) #'ivy--regex-ignore-order)
    ))
;; counsel
(use-package counsel
  :ensure t
  :diminish
  :hook (ivy-mode . counsel-mode)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("M-y" . counsel-yank-pop)
         ("C-x b" . counsel-switch-buffer)
         ("M-r" . counsel-rg))
  :custom
  (counsel-yank-pop-separator "\n--------\n")
  (kill-ring-max 1000)
  :config
  (setq ivy-initial-inputs-alist '((t . ""))))
;; swiper
(use-package swiper
  :ensure t
  :bind (("M-i" . swiper-thing-at-point)
         :map swiper-map
         ("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)))
;;;; avy, ace
;; avy
(use-package avy
  :ensure t
  :bind (("C-^" . avy-goto-char-timer)
         ("C-]" . avy-goto-line)))
(use-package avy-migemo
  :ensure t
  :bind (("C-^" . avy-migemo-goto-char-timer))
  :init
  (defun my:ivy-migemo-re-builder (str)
    "Own ivy-migemo-re-build for swiper."
    (let* ((sep " \\|\\^\\|\\.\\|\\*")
           (splitted (--map (s-join "" it)
                            (--partition-by (s-matches-p " \\|\\^\\|\\.\\|\\*" it)
                                            (s-split "" str t)))))
      (s-join "" (--map (cond ((s-equals? it " ") ".*?")
                              ((s-matches? sep it) it)
                              (t (migemo-get-pattern it)))
                        splitted))))
  :custom
  (avy-migemo-at-full-max 2)
  ;; (use-package avy-migemo-e.g.swiper)
  )
;; ace-window
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?h ?j ?k ?l ?u ?i ?o ?p))
  :custom-face
  (aw-leading-char-face ((t (:height 2.0 :forground "#f1fa8c")))))
;;;; smex
(use-package smex
  :ensure t
  :custom
  (smex-history-length 32)
  (smex-save-file (expand-file-name "smex-items" my:d:tmp)))
;;;; company
(use-package company
  :ensure t
  :diminish company-mode
  :hook (after-init . global-company-mode)
  ;;(emacs-lisp-mode . ,(lambda () (add-to-list 'company-backends 'company-elisp))))
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<tab>" . company-complete-selection)
              :map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :custom
  (company-transformers '(company-sort-by-backend-importance))
  (company-idle-delay 0.01)
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 1)
  (completion-ignore-case t)
  (company-show-numbers t)
  :config
  (add-hook 'emacs-lisp-mode-hook #'(lambda () (add-to-list 'company-backends 'company-elisp)))
  (use-package company-flx
    :ensure t
    :hook (company-mode . company-flx-mode))
  (use-package company-prescient
    :ensure t
    :hook (company-mode . company-prescient-mode))
  (use-package company-box
    :ensure t
    :no-require t
    :hook (company-mode . company-box-mode)
    :custom
    (company-box-icons-alist 'company-box-icons-all-the-icons)
    :config
    (setq company-box-icons-unknown 'fa_question_circle)
    (setq company-box-icons-elisp
          '((fa_tag :face font-lock-function-name-face) ;; Function
            (fa_cog :face font-lock-variable-name-face) ;; Variable
            (fa_cube :face font-lock-constant-face) ;; Feature
            (md_color_lens :face font-lock-doc-face))) ;; Face
    (setq company-box-icons-yasnippet 'fa_bookmark)
    (setq company-box-icons-lsp
          '((1 . fa_text_height) ;; Text
            (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
            (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
            (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
            (5 . (fa_cog :foreground "#FF9800")) ;; Field
            (6 . (fa_cog :foreground "#FF9800")) ;; Variable
            (7 . (fa_cube :foreground "#7C4DFF")) ;; Class
            (8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
            (9 . (fa_cube :foreground "#7C4DFF")) ;; Module
            (10 . (fa_cog :foreground "#FF9800")) ;; Property
            (11 . md_settings_system_daydream) ;; Unit
            (12 . (fa_cog :foreground "#FF9800")) ;; Value
            (13 . (md_storage :face font-lock-type-face)) ;; Enum
            (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
            (15 . md_closed_caption) ;; Snippet
            (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
            (17 . fa_file_text_o) ;; File
            (18 . md_refresh) ;; Reference
            (19 . fa_folder_open) ;; Folder
            (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
            (21 . (fa_square :face font-lock-constant-face)) ;; Constant
            (22 . (fa_cube :face font-lock-type-face)) ;; Struct
            (23 . fa_calendar) ;; Event
            (24 . fa_square_o) ;; Operator
            (25 . fa_arrows)) ;; TypeParameter
          )
    )
  (use-package company-quickhelp
    :ensure t
    :hook (company-mode . company-quickhelp-mode))
  (use-package company-tabnine
    :ensure t
    :config
    (add-to-list 'company-backends #'company-tabnine :append)
    ;; (company-tabnine-install-binary)
    ))

;;;; Tab
;; Tab-bar-mode or Elscreen
(defvar tab-bar-p (version<= "27" emacs-version))
(use-package tab-bar
  :if tab-bar-p
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-name-function
   #'(lambda () (concat "ǀ " (tab-bar-tab-name-current-with-count))))
  :init
  (defvar my-tab-bar-map (make-sparse-keymap))
  (bind-keys :prefix-map my-tab-bar-map
             :prefix "C-z"
             ("c" . tab-new)
             ("C-c" . tab-new)
             ("k" . tab-close)
             ("C-k" . tab-close)
             ("n" . tab-next)
             ("C-n" . tab-next)
             ("p" . tab-previous)
             ("C-p" . tab-previous)
             ("z" . tab-recent)
             ("C-z" . tab-recent)))
;; Centaur tabs
(use-package centaur-tabs
  :ensure t
  :hook (after-init . centaur-tabs-mode)
  :custom
  (centaur-tabs-prefix-key (kbd "M-z"))
  (centaur-tabs-style "bar")
  (centaur-tabs-height 12)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-bar 'left)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-cycle-scope 'tabs)
  :config
  (centaur-tabs-group-by-projectile-project)
  (bind-keys
   ("C-<tab>" . centaur-tabs-forward)
   ("<C-S-iso-lefttab>" . centaur-tabs-backward)
   :prefix-map centaur-tabs-prefix-map
   :prefix "M-z"
   ("n" . centaur-tabs-forward)
   ("C-n" . centaur-tabs-forward)
   ("M-n" . centaur-tabs-forward)
   ("p" . centaur-tabs-backward)
   ("C-p" . centaur-tabs-backward)
   ("M-p" . centaur-tabs-backward)
   ("k" . kill-current-buffer)
   ("C-k" . kill-current-buffer)
   ("M-k" . kill-current-buffer)
   ("f" . centaur-tabs-forward-group)
   ("C-f" . centaur-tabs-forward-group)
   ("M-f" . centaur-tabs-forward-group)
   ("b" . centaur-tabs-backward-group)
   ("C-b" . centaur-tabs-backward-group)
   ("M-b" . centaur-tabs-backward-group)
   ("C-a" . centaur-tabs-select-beg-tab)
   ("C-e" . centaur-tabs-select-end-tab)))

;;; Text
;;;; migemo
(defvar migemo-exists-p
  (eval-when-compile
    (and (executable-find "cmigemo")
         (locate-library "migemo"))))
(use-package migemo
  :ensure t
  :if migemo-exists-p
  :custom
  (migemo-coding-system 'utf-8-unix)
  (migemo-command "cmigemo")
  (migemo-dictionary
   (or (my:path-exists? "/usr/share/cmigemo/utf-8/migemo-dict")
       (my:path-exists? "/usr/share/migemo/migemo-dict")))
  (migemo-regex-dictionary nil)
  (migemo-user-dictionary nil)
  (migemo-options '("-q" "--emacs"))
  :config
  (migemo-init))
;;;; IME -- ddskk
(defvar my:d:skk (expand-file-name "app/SKK" my:d:nextcloud))
(setq skk-large-jisyo (expand-file-name "SKK-JISYO.L+emoji.utf8" my:d:skk))
(use-package skk
  :ensure ddskk
  :bind (([hiragana-katakana] . skk-mode)
         ("C-\\" . skk-mode))
  :init
  (setq skk-user-directory my:d:skk
        skk-jisyo (expand-file-name "jisyo.utf8" my:d:skk)
        skk-backup-jisyo (expand-file-name "SKK/jisyo.bak" my:d:tmp)
        skk-study-backup-file (expand-file-name "SKK/study.bak" my:d:tmp))
  (setq skk-jisyo-code 'utf-8)
  (setq default-input-method "japanese-skk")
  :config
  (setq skk-extra-jisyo-file-list
        (list
         (expand-file-name "SKK-JISYO.JIS3_4" my:d:skk)))
  ;; (skk-toggle-katakana 'jisx0201-kana)
  (setq default-input-method
        "japanese-skk" ; (skk-mode 1)
        ;;    "japanese-skk-auto-fill"		; (skk-auto-fill-mode 1)
        )
  (setq skk-isearch-mode-enable 'always)
  (setq skk-preload nil)
  (use-package skk-study)
  ;; 数値変換機能を使う
  (setq skk-use-numeric-conversion t)
  ;; 半角カナ入力メソッドを使う
  (setq skk-use-jisx0201-input-method t)
  ;; 辞書アップデート
  (setq skk-update-jisyo-function
        #'(lambda (word &optional purge)
            (if purge
                (skk-update-jisyo-original word purge)
              (let* ((pair (skk-treat-strip-note-from-word word))
                     (cand (car pair))
                     (note (cdr pair)))
                (when (and (stringp note)
                           (> (length note) 8))
                  ;; 注釈が 8 文字より長かったら注釈を消して登録する
                  (setq note nil))
                (setq word (if (stringp note)
                               (concat cand ";" note)
                             cand))
                (skk-update-jisyo-original word)))))
  ;; Enter キーで確定 (改行しない)
  (setq skk-egg-like-newline t)
  ;; 句読点にコンマとピリオドを使用する
  (setq skk-kuten-touten-alist
        (cons '(my-jp "．" . "，")
              (cons '(my-en ". " . ", ")
	                skk-kuten-touten-alist)))
  (setq-default skk-kutouten-type 'my-jp))
;;;; Spell Checker
;; ispell
(use-package ispell
  :custom
  (ispell-program-name "hunspell")
  :config
  (defvar ispell-regexp-ja "[一-龠ぁ-🈀ァ-𛀀ー・、。々]+"
    "Regular expression to match a Japanese word.
The expression can be [^\000-\377]+, [^!-~]+, or [一-龠ぁ-🈀ァ-𛀀ー・、。々]+")
  ;; (add-to-list 'ispell-skip-region-alist (list ispell-regexp-ja))
  (defun flyspell-skip-ja (beg end info)
    "Tell flyspell to skip a Japanese word.
Call this on `flyspell-incorrect-hook'."
    (string-match ispell-regexp-ja (buffer-substring beg end)))
  (add-hook 'flyspell-incorrect-hook 'flyspell-skip-ja))
;; flyspell
(use-package flyspell
  :hook ((org-mode text-mode LaTeX-mode) . flyspell-mode)
  :config
  (defvar flyspell-correct-map (make-sparse-keymap))
  (use-package flyspell-correct-avy-menu
    :ensure t
    :init
    (bind-keys :prefix-map flyspell-correct-map
               :prefix "C-c"
               ("s" . flyspell-correct-wrapper))))
;;;; Translater
(use-package google-translate
  :ensure t
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja")
  :config
  (bind-keys
   :prefix-map google-translate-map
   :prefix "M-t"
   ("M-t" . google-translate-at-point)
   ("M-r" . google-translate-at-point-reverse)))

;;; Common Packages
;;;; eww
(use-package eww
  :preface
  (setq my:d:eww (expand-file-name "eww" my:d:tmp))
  :custom
  (eww-bookmarks-directory my:d:eww)
  (eww-history-limit 9999)
  (eww-search-prefix "https://www.google.com/search?q="))
;;;; undo
;; undohist
(use-package undohist
  :ensure t
  :config
  (setq undohist-directory (expand-file-name "undohist" my:d:tmp))
  (undohist-initialize))
;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode))
;;;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :after company
  :hook (prog-mode . yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (push (expand-file-name "snippets" my:d:share) yas-snippet-dirs)
  (defvar company-mode/enable-yas t)
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (add-hook 'yas-minor-mode
            #'(lambda ()
                (setq-local
                 company-backends
                 (mapcar #'company-mode/backend-with-yas company-backends)))))

;;; VCS -- Git
(setq vc-follow-symlinks t)
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))
(use-package git-gutter
  :ensure t
  :hook (after-init . global-git-gutter-mode)
  :custom
  (git-gutter:handled-backends '(git hg))
  :custom-face
  (git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
  (git-gutter:added    ((t (:foreground "#50fa7b" :background "#50fa7b"))))
  (git-gutter:deleted  ((t (:foreground "#ff79c6" :background "#ff79c6")))))
(use-package gitconfig-mode
  :ensure t
  :defer t)
(use-package gitignore-mode
  :ensure t
  :defer t)
(use-package counsel-ghq
  :quelpa (counsel-ghq :fetcher github :repo windymelt/counsel-ghq)
  :bind ("M-g" . counsel-ghq))

;;; Programming Language
;;;; projectile
(use-package projectile
  :ensure t
  :after counsel
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  (projectile-cache-file (expand-file-name "projectile.cache" my:d:tmp))
  (projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" my:d:tmp))
  :config
  (my:enable-mode projectile-mode)
  (defun select-ssh-hostname ()
    (interactive)
    (defun extract-hosts ()
      (let* ((extract "grep '^Host ' | cut -d' ' -f2- | grep -Eo '[^ ]+' | sort -u | grep -v '\*'")
             (ssh-dir (expand-file-name ".ssh" (getenv "HOME")))
             (find-cat-pipe (concat "find " ssh-dir " -type f | xargs cat | ")))
        (shell-command-to-string (concat find-cat-pipe extract))))
    (ivy-read "[host] " (split-string (extract-hosts) "\n")))
  (defun projectile-reposyncer-push ()
    (interactive)
    (if (executable-find "reposyncer")
        (shell-command (concat "reposyncer push " (select-ssh-hostname) " " (projectile-acquire-root)))
      (message "reposyncer not found.")))
  (defun projectile-reposyncer-pull ()
    (interactive)
    (if (executable-find "reposyncer")
        (shell-command (concat "reposyncer pull " (select-ssh-hostname) " " (projectile-acquire-root)))
      (message "reposyncer not found.")))
  (use-package counsel-projectile
    :ensure t
    :if (featurep 'counsel)
    :bind
    (:map projectile-mode-map
          ("M-p" . projectile-command-map)
          ("C-c p" . projectile-command-map)
          ("C-c C-p" . projectile-command-map)
          :map projectile-command-map
          ("s" . counsel-projectile-rg))))

;;;; Neotree
(use-package neotree
  :ensure t
  :after projectile
  :bind (("<f8>" . neotree-toggle)
         :map neotree-mode-map
         ("a" . neotree-hidden-file-toggle)
         ("^" . neotree-select-up-node)
         ("<right>" . neotree-change-root))
  :custom
  (neo-theme 'icons)
  (neo-smart-open t)
  (neo-keymap-style 'concise)
  (neo-vc-integration t))

;;;; flycheck
(use-package flycheck
  :ensure t
  :hook ((python-mode) . flycheck-mode)
  :config
  ;; (use-package flycheck-color-mode-line
  ;;   :ensure t
  ;;   :hook (flycheck-mode . flycheck-color-mode-line-mode))
  ;; (use-package flycheck-popup-tip
  ;;   :ensure t
  ;;   :hook (flycheck-mode . flycheck-popup-tip-mode))
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;;;; LSP
(use-package lsp-mode
  :ensure t
  :bind (("M-." . xref-find-definitions)
         ("M-," . xref-pop-marker-stack)
         ("M-/" . xref-find-references))
  :commands lsp
  :hook ((python-mode sh-mode c++-mode rust-mode) . #'lsp)
  :custom
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  (lsp-auto-guess-root t)
  (lsp-enable-completion-at-point t)
  (lsp-response-timeout 5)
  (lsp-idle-delay 0.5)
  (lsp-prefer-flymake nil)
  (lsp-prefer-capf t)
  (lsp-enable-snippet t)
  (lsp-session-file (expand-file-name "lsp-session-v1" my:d:tmp))
  (lsp-server-install-dir (expand-file-name "lsp" my:d:tmp))
  (lsp-rust-server 'rust-analyzer)
  :config
  (setq lsp-restart 'auto-restart)
  (setq lsp-completion-provider :capf)
  (setq lsp-document-sync-method lsp--sync-incremental)
  (use-package company-capf
    :after (company lsp-mode)
    :config
    (push 'company-capf company-backends))
  (use-package lsp-ui
    :ensure t
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-max-width 60)
    (lsp-ui-doc-max-height 20)))

;;;; C, C++
;; (use-package cquery
;;   :if (executable-find "cquery")
;;   :ensure t
;;   :config
;;   (setq cquery-executablez (executable-find "cquery")))
(use-package ccls
  :if (executable-find "ccls")
  :ensure t
  :config
  (setq ccls-executable (executable-find "ccls")))

;;;; bison, flex
(use-package bison-mode
  :ensure t
  :mode
  (("\.\(y\|yy\)$" . bison-mode)
   ("\.\(l\|ll\)$" . flex-mode)))

;;;; Rust
(use-package rust-mode
  :ensure t
  :custom (rust-format-on-save t))
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;;;; shell script
(use-package fish-mode
  :ensure t
  :defer t)
(use-package ebuild-mode
  :if my:gentoo-p
  :mode
  (("make.conf" . shell-script-mode)))

;;;; YAML
(use-package yaml-mode
  :ensure t
  :custom
  (yaml-indent-offset 4))

;;;; TeX
(use-package yatex
  :ensure t
  :mode
  (("\\.tex$" . yatex-mode)
   ("\\.sty$" . yatex-mode)
   ("\\.ltx$" . yatex-mode))
  :init
  (add-hook 'yatex-mode-hook 'turn-on-reftex)
  :config
  (setq tex-command "latexmk")
  (setq YaTeX-kanji-code 4)
  (setq tex-pdfview-command (executable-find "okular"))
  (setq electric-indent-mode nil)
  (setq reftex-use-external-file-finders t))
;; PDF
(use-package pdf-tools
  :ensure t
  :mode
  (("\\.pdf$" . pdf-view-mode))
  :custom
  (pdf-view-resize-factor 1.1)
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (my:disable-mode display-line-numbers-mode)))
  (bind-keys :map pdf-view-mode-map ("C-s" . isearch-forward)))

;;;; Lisp -- Common Lisp, Scheme
;;;;; Common Lisp
;; SLIME
(defvar quicklisp-directory
  (my:path-exists? (my:locate-home "quicklisp")))
(use-package slime
  :if quicklisp-directory
  :ensure slime-company
  :config
  (load (expand-file-name "slime-helper.el" quicklisp-directory))
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-company)))
;;;;; Scheme
(when (executable-find "gosh")
  (setq scheme-program-name "gosh -i")
  (defun scheme-other-window ()
    "Run Gauche on other window"
    (interactive)
    (split-window-horizontally (/ (frame-width) 2))
    (let ((buf-name (buffer-name (current-buffer))))
      (scheme-mode)
      (switch-to-buffer-other-window
       (get-buffer-create "*scheme*"))
      (run-scheme scheme-program-name)
      (switch-to-buffer-other-window
       (get-buffer-create buf-name))))
  (bind-key "C-c S" 'scheme-other-window))

;;;; Web
(use-package emmet-mode
  :ensure t
  :hook (((sgml-mode css-mode html-mode) . emmet-mode)
         (emmet-mode . (lambda () (keyboard-translate ?\C-i ?\H-i))))
  :bind (:map emmet-mode-keymap
              ("H-i" . emmet-expand-line))
  :config
  (use-package web-mode
    :ensure t)
  (setq emmet-indentation 2))

;;;; Ruby
;; enh-ruby-mode
(use-package enh-ruby-mode
  :ensure t
  :if (executable-find "ruby")
  :mode
  (("\\.rb$" . enh-ruby-mode)
   ("\\.rake$" . enh-ruby-mode)
   ("\\.cap$" . enh-ruby-mode)
   ("config.ru$" . enh-ruby-mode)
   ("Rakefile$" . enh-ruby-mode)
   ("Capfile$" . enh-ruby-mode)
   ("Gemfile$" . enh-ruby-mode))
  :config
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
  (defun my:ruby-mode-hook-function ()
    (setq enh-ruby-deep-indent-paren nil)
    (setq enh-ruby-deep-indent-paren-style nil)
    (setq enh-ruby-use-encoding-map nil)
    (lambda () (ruby-electric-mode t))
    (setq ruby-electric-expand-delimiters-list nil)
    (lambda () (ruby-block-mode t)
      (setq ruby-block-highlight-toggle t)))
  (add-hook 'enh-ruby-mode-hook 'my:ruby-mode-hook-function)
  ;; 保存時にmagic commentを追加しないようにする
  (defadvice enh-ruby-mode-set-encoding (around stop-enh-ruby-mode-set-encoding)
    "If enh-ruby-not-insert-magic-comment is true, stops enh-ruby-mode-set-encoding."
    (if (and (boundp 'enh-ruby-not-insert-magic-comment)
             (not enh-ruby-not-insert-magic-comment))
        ad-do-it))
  (ad-activate 'enh-ruby-mode-set-encoding)
  (setq-default enh-ruby-not-insert-magic-comment t))
;; robe
(use-package robe
  :ensure t
  :ensure-system-package (pry . "gem install pry")
  :if (executable-find "pry")
  :hook ((enh-ruby-mode . robe-mode)))
;; rubocop, ruby-lint
(use-package rubocop
  :ensure t
  :ensure-system-package ((rubocop . "gem install rubocop")
                          (ruby-lint . "gem install ruby-lint"))
  :if (executable-find "rubocop")
  :hook (enh-ruby-mode . rubocop-mode)
  :config
  ;; definition for flycheck
  (flycheck-define-checker ruby-rubylint
    "A Ruby syntax and style checker using the rubylint tool."
    :command ("ruby-lint" source)
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
              line-end)
     (error line-start
            (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
            line-end))
    :modes (enh-ruby-mode ruby-mode))
  (flycheck-define-checker ruby-rubocop
    "A Ruby syntax and style checker using the RuboCop tool."
    :command ("rubocop" "--format" "emacs" "--silent"
              (config-file "--config" flycheck-rubocoprc)
              source)
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
              line-end)
     (error line-start
   	        (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
            line-end))
    :modes (enh-ruby-mode motion-mode)))
;; inf-ruby
(use-package inf-ruby
  :defer t
  :hook (enh-ruby-mode . inf-ruby-minor-mode))

;;;; python
;; python-mode
(use-package python-mode
  :ensure t
  :config
  (setq py-outline-minor-mode-p nil)
  (setq py-current-defun-show nil)
  (setq py-jump-on-exception nil)
  (setq py-current-defun-delay 1000)
  ;; poetry
  (use-package poetry
    :ensure t
    :bind (:map python-mode-map
                ("C-x p" . poetry))
    :custom
    (poetry-tracking-strategy 'projectile))
  ;; (use-package lsp-python-ms
  ;;   :ensure t
  ;;   :init (setq lsp-python-ms-auto-install-server t))
  (use-package lsp-pyright
    ;; npm install -g pyright
    :ensure t
    :hook (python-mode . (lambda () (poetry-tracking-mode 1) (require 'lsp-pyright) (lsp))))
  ;; python-black
  (use-package python-black
    :ensure t)
  ;; py-isort
  (use-package py-isort
    :ensure t)

  (defun python-formatter ()
    (interactive)
    (py-isort-buffer)
    (python-black-buffer))

  (defun my-python-mode-hook ()
    (my:enable-mode poetry-tracking-mode)
    (require 'lsp-python-ms)
    (lsp)
    (bind-keys :map python-mode-map
               ("C-c f" . python-formatter)
               ("C-c C-f" . python-formatter)))
  (add-hook 'python-mode-hook #'my-python-mode-hook)
  (setq lsp-pyls-plugins-pylint-enabled t)
  (setq lsp-pyls-plugins-autopep8-enabled nil)
  (setq lsp-pyls-plugins-yapf-enabled nil))

;; quickrun
(use-package quickrun
  :ensure t
  :config
  (quickrun-add-command "python"
    '((:command . "python"))
    ;; :override t)
    )
  (bind-keys :map python-mode-map
             ("C-c q" . quickrun))
  (use-package popwin
    :config
    (push '("*quickrun*") popwin:special-display-config)))

;;;; outline-(minor-)?mode
(use-package outline
  :bind (:map outline-minor-mode-map
              ("<tab>" . outline-cycle)
              ("C-c C-f" . outline-forward-same-level)
              ("C-c C-b" . outline-backward-same-level)
              ("C-c C-n" . outline-next-visible-heading)
              ("C-c C-p" . outline-previous-visible-heading)
              :map outline-mode-map
              ("<tab>" . outline-cycle))
  :config
  (use-package outline-magic
    :ensure t))

;;; To Work
;;;; Wanderlust -- E-mail client:
(use-package wl
  :ensure wanderlust
  :commands (wl)
  :config
  (use-package mime-def)
  (use-package cp5022x
    :ensure t
    :config
    (add-to-list 'mime-charset-coding-system-alist '(iso-2022-jp . cp50220)))
  (setq wl-mime-charset 'utf-8)
  (setq mime-situation-examples-file (expand-file-name "mime-example" my:d:tmp))
  (use-package mime-setup
    :preface
    (setq mime-view-text/html-previewer 'shr
          mime-setup-enable-inline-image 'shr))
  (use-package rail
    :quelpa (rail :fetcher github :repo uwabami/rail)
    :init
    (setq rail-emulate-genjis t))
  (use-package elscreen-wl
    :if (featurep 'elscreen))
  ;; prefer text/plain than html
  (set-alist 'mime-view-type-subtype-score-alist '(text . html) 0)
  (setq wl-init-file (my:locate-home ".mua/wl-info.el")))

;;;; org-mode
(use-package org
  :mode
  (("\\.org$" . org-mode))
  :bind (("C-c l" . org-store-link)
         ("C-c o" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c t" . org-todo))
  :hook (org-mode . turn-on-font-lock)
  :init
  (setq org-directory (expand-file-name "org/" my:d:nextcloud))
  :custom
  (org-latex-pdf-process '("latexmk %f"))
  (org-export-in-background t)
  (org-export-async-debug t)
  :config
  (use-package org-install)
  (use-package org-capture)
  (use-package org-protocol)
  (use-package ox)
  (use-package ox-latex)
  (use-package ox-beamer)
  (use-package ox-gfm
    :ensure t)
  (use-package ox-rst
    :ensure t)
  (setq org-capture-bookmark nil)
  (setq org-startup-truncated nil)
  (setq org-return-follows-link t)
  ;;(org-remember-insinuate)
  (setq org-default-notes-file (expand-file-name "agenda.org" org-directory))
  (setq org-capture-templates
        '(("t" "Todo" entry (file "todo.org")
           "* TODO %?%i\n  %T"
           :kill-buffer t)
          ("m" "memo" entry (file "memo.org")
           "* %?%i\n  %a\n  %T"
           :kill-buffer t)
          ("b" "bookmarks" entry (file+headline "memo.org" "Bookmarks")
           "* %:description\n    %?\n    %:link\n    %T"
           :kill-buffer t)
          ("s" "Snippets" entry (file (expand-file-name "snippets.org" org-directory))
           "* %T\n#+begin_src \n%i%?\n#+end_src"
           :kill-buffer t)
          ("d" "diary" entry (file+datetree "diary.org")
           "* %?%i\n"
           :kill-buffer t)
          ("j" "journal log" entry (file+datetree "journal.org")
           "* %?%i\n  [%<%Y-%m-%d (%a) %H:%M:%S>]"
           :kill-buffer t)
          ("a" "Daily" entry (file+datetree "journal.org")
           "* TODO %?%i :daily:\n  %t"
           :kill-buffer t)
          ("r" "research note" entry (file "research.org")
           "* %?%i\n  %a\n  <%<%Y-%m-%d (%a) %H:%M:%S>>"
           :kill-buffer t)))
  (setq org-tag-alist '(("daily" . ?d)))
  (setq org-agenda-files nil)
  (dolist (file '("todo.org" "memo.org" "diary.org" "journal.org" "schedule.org" "research.org"))
    (push (expand-file-name file org-directory) org-agenda-files))
  (setq org-export-with-toc nil)
  (setq org-duration-format (quote h:mm))
  ;; org-gcal
  (use-package request
    :ensure t
    :init
    (setq request-storage-directory (expand-file-name "request" my:d:tmp))
    (unless (file-directory-p request-storage-directory)
      (make-directory request-storage-directory)))
  (use-package org-gcal
    :ensure t
    :if (file-directory-p org-directory)
    :commands (org-gcal-fetch org-gcal-sync)
    :custom
    (org-generic-id-locations-file (expand-file-name "org-generic-id-locations" my:d:tmp))
    :init
    (setq org-gcal-dir (expand-file-name "org-gcal" my:d:tmp))
    (unless org-gcal-dir
      (make-directory org-gcal-dir))
    (setq org-gcal-token-file (expand-file-name ".org-gcal-token" org-gcal-dir))
    (setq alert-log-messages t)
    (setq alert-default-style 'log)
    (setq org-gcal-down-days   90) ;; 過去 3 month
    (setq org-gcal-up-days    180) ;; 未来 6 month
    (setq org-gcal-auto-archive nil)
    :config
    (load (expand-file-name "app/org-gcal/token" my:d:nextcloud))
    (setq org-gcal-file-alist `(("de9uch1@gmail.com" . ,(expand-file-name "schedule.org" org-directory))))))

;;; Misc Packages
;;;; twittering-mode
(use-package twittering-mode
  :ensure t
  :commands twit
  :config
  (setq twittering-icon-mode t)                ; Show icons
  (setq twittering-timer-interval 300)         ; Update your timeline each 300 seconds (5 minutes)
  (setq twittering-convert-fix-size 35)
  (setq twittering-use-master-password t)
  (setq twittering-private-info-file (my:locate-home ".gnupg/twittering-mode.gpg"))
  (setq twittering-icon-storage-limit t)
  (setq twittering-icon-storage-file (expand-file-name "twmode-icon" my:d:tmp))
  (setq twittering-connection-type-order '(wget curl urllib-http native urllib-https))
  ;; ふぁぼるとき確認しない
  (defun my:twittering-favorite (&optional remove)
    (interactive "P")
    (let ((id (get-text-property (point) 'id))
          (text (copy-sequence (get-text-property (point) 'text)))
          (method (if remove 'destroy-favorites 'create-favorites)))
      (set-text-properties 0 (length text) nil text)
      (twittering-call-api method `((id . ,id)))))
  ;; key binding
  (bind-keys :map twittering-mode-map
             ("F" . my:twittering-favorite) ; "F" で fav
             ("R" . twittering-native-retweet)) ; "R" で RT
  ;; Default Format
  (setq twittering-status-format-default
        "%i %s,  %@:
 %FILL[  ]{%T // from %f%L%r%R}
  ")
  ;; Custom Format
  (setq twittering-status-format
        "%i @%s  %S %p  [%C{%y/%m/%d %H:%M:%S}]
%FOLD[  ]{%T}
%FILL[        ]{via: %f %r %R }")
  (setq twittering-retweet-format " RT @%s: %t"))
;;;; SSH
;; ssh-config-mode
(use-package ssh-config-mode
  :ensure t
  :custom
  (ssh-config-mode-indent 4))
;;;; which-key
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))
;;;; open-junk-file
(use-package open-junk-file
  :ensure t
  :bind (("C-x j" . open-junk-file))
  :config
  (setq open-junk-file-format (my:locate-home "tmp/junk/%Y-%m-%d-%H%M%S.")))
;;;; recentf
(use-package recentf
  :custom
  (recentf-save-file (expand-file-name "recentf" my:d:tmp))
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 5000)
  (recentf-auto-cleanup 100)
  (recentf-exclude '(".recentf" "COMMIT_EDITMSG" "/\\.emacs\\.d/elpa/"))
  :hook (after-init . recentf-mode)
  :config
  (run-with-idle-timer 30 t '(lambda () (with-suppressed-message (recentf-save-list))))
  (use-package recentf-ext
    :ensure t))
;;;; mwim
(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))
;;;; anzu
(use-package anzu
  :ensure t
  :diminish
  :hook (after-init . global-anzu-mode)
  :custom
  (anzu-use-migemo t)
  (anzu-search-threshold 3000))
;;;; highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :hook ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?ǀ))
;;;; expand-region
(use-package expand-region
  :ensure t
  :bind (("C-SPC" . er/expand-region)
         ("C-M-SPC" . er/contract-region)
         ("C-@" . er/expand-region)
         ("C-M-@" . er/contract-region))
  :custom
  (expand-region-smart-cursor t)
  :config
  (setq shift-select-mode nil)
  (setq transient-mark-mode t))
;;;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-M-c" . mc/edit-lines)
         ("C-M-t" . mc/mark-all-in-region)
         ("C-." . mc/mark-next-like-this)
         ("<C-f11>" . mc/mark-next-like-this)
         ("C-," . mc/mark-previous-like-this)
         ("<C-f10>" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (setq mc/list-file (expand-file-name "mc-lists.el" my:d:tmp)))
;;;; smartparens
(use-package smartparens
  :ensure t
  :diminish
  :hook (after-init . smartparens-global-mode)
  :config
  (use-package smartparens-config))
;;;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))
;;;; rainbow-mode
(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))
;;;; multi-term
(use-package multi-term
  :ensure t
  :commands multi-term
  :config
  (setq multi-term-program (executable-find "bash")))
;;;; vterm
(use-package vterm
  :ensure t
  :custom
  (vterm-max-scrollback 10000000)
  (vterm-buffer-name-string "vterm: %s")
  (vterm-keymap-exceptions '("<f1>" "<f12>" "C-c" "C-x" "C-u" "C-g" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y"))
  :config
  ;; Workaround of not working counsel-yank-pop
  ;; https://github.com/akermu/emacs-libvterm#counsel-yank-pop-doesnt-work
  (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
    (if (equal major-mode 'vterm-mode)
        (let ((inhibit-read-only t)
              (yank-undo-function (lambda (_start _end) (vterm-undo))))
          (cl-letf (((symbol-function 'insert-for-yank)
                     (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
      (apply orig-fun args)))
  (advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action))
(use-package vterm-toggle
  :ensure t
  :bind ("<f12>" . vterm-toggle)
  :custom
  (vterm-toggle-scope 'project))


;;; Profiler
;; (profiler-report)
;; (profiler-stop)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8-unix
;; mode: emacs-lisp
;; mode: outline-minor
;; time-stamp-pattern: "10/Modified:\\\\?[ \t]+%:y-%02m-%02d\\\\?\n"
;; End:

;;; init.el ends here
