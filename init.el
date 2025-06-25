;; -*- lexical-binding: t -*-
;;; init.el --- Emacs Initialization File

;; Filename: init.el
;; Description: my emacs configuration
;; Package-Requires: ((emacs "26.1"))
;; Author: Hiroyuki Deguchi <deguchi.hiroyuki.db0@is.naist.jp>
;; Created: 2018-05-26
;; Modified: 2025-06-25
;; Version: 0.0.5
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
;; Setup tracker
(defvar setup-tracker--level 0)
(defvar setup-tracker--parents nil)
(defvar setup-tracker--times nil)
(when load-file-name
  (push load-file-name setup-tracker--parents)
  (push (current-time) setup-tracker--times)
  (setq setup-tracker--level (1+ setup-tracker--level)))
(add-variable-watcher
 'load-file-name
 (lambda (_ v &rest __)
   (cond ((equal v (car setup-tracker--parents))
          nil)
         ((equal v (cadr setup-tracker--parents))
          (setq setup-tracker--level (1- setup-tracker--level))
          (let* ((now (current-time))
                 (start (pop setup-tracker--times))
                 (elapsed (+ (* (- (nth 1 now) (nth 1 start)) 1000)
                             (/ (- (nth 2 now) (nth 2 start)) 1000))))
            (with-current-buffer (get-buffer-create "*setup-tracker*")
              (save-excursion
                (goto-char (point-min))
                (dotimes (_ setup-tracker--level) (insert "> "))
                (insert
                 (file-name-nondirectory (pop setup-tracker--parents))
                 " (" (number-to-string elapsed) " msec)\n")))))
         (t
          (push v setup-tracker--parents)
          (push (current-time) setup-tracker--times)
          (setq setup-tracker--level (1+ setup-tracker--level))))))
;; Measure the initialization time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "init time: %.1f msec"
                     (* (float-time (time-subtract after-init-time before-init-time)) 1000))))
;;; My Functions and Macros -- prefix "my:"
(defmacro my:if! (pred then &rest else)
  "Expand if-statement on the compile time."
  (declare (indent 2))
  (if (eval pred) then `(progn ,@else)))
(defmacro my:unless! (pred &rest body)
  "Expand if-statement on the compile time."
  (declare (indent 1))
  (if (not (eval pred)) `(progn ,@body)))
(defmacro my:path-exists? (path)
  "Return PATH if PATH exists else nil."
  (if (file-exists-p path) path nil))
(defmacro my:locate-user-emacs-file (x)
  "Expand filename (locate-user-emacs-file X)."
  (expand-file-name (locate-user-emacs-file x)))
;; my:locate-home
(defmacro my:locate-home (x)
  "Concat and expand path X from the home directory."
  (expand-file-name x "~"))
;; mode enable/disable
(defmacro my:enable-mode (mode)
  "Enable MODE."
  `(,mode 1))
(defmacro my:disable-mode (mode)
  "Disable MODE."
  `(,mode 0))
;; suppressed message
(defun suppress-messages (func &rest args)
  "Suppress message output from FUNC."
  ;; Some packages are too noisy.
  ;; https://superuser.com/questions/669701/emacs-disable-some-minibuffer-messages
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply func args)
      (advice-remove 'message #'silence))))

;;; Tuning and Speed Up:
(eval-and-compile (defconst early-init-compat (version<= "27.1" emacs-version)))
(my:unless! early-init-compat
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
(eval-when-compile (require 'cl-lib nil t))
(eval-and-compile
  (setq byte-compile-warnings '(not cl-functions free-vars docstrings unresolved))
  (my:if! (and (fboundp 'native-comp-available-p)
               (native-comp-available-p))
      (setq native-comp-speed 3
            native-comp-async-jobs-number 4
            native-comp-async-report-warnings-errors 'silent)))
(setq package-native-compile t)
;; (native-compile-async (locate-user-emacs-file "early-init.el"))
;; (native-compile-async (locate-user-emacs-file "init.el"))

;;; System Local
;; (defvar my:distrib-id nil)
;; (defvar my:gentoo-p nil)
;; (eval-and-compile
;;   (when (file-exists-p "/etc/gentoo-release")
;;     (setq my:distrib-id "gentoo")
;;     (setq my:gentoo-p t)))

;;; My Configurations
;; Name
(setq user-full-name "Hiroyuki Deguchi")
;; E-mail Address
(setq user-mail-address "hiroyuki.deguchi@ntt.com")
;;;; Directory --
;; $HOME/.emacs.d
;; for ``emacs -q -l .emacs''
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(eval-and-compile (defconst my:d:tmp (my:locate-user-emacs-file "tmp")))
(eval-and-compile (defconst my:d:share (my:locate-user-emacs-file "share")))
;; Nextcloud
(eval-and-compile
  (defconst my:d:nextcloud
    (let ((d (my:locate-home "Nextcloud")))
      (if (file-exists-p d)
          d
        user-emacs-directory))))

;;; Package Management
;;;; package.el
(eval-and-compile
  (setq
   package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (setq package-quickstart t)
  (setq package-quickstart-file (expand-file-name "package-quickstart.el" my:d:tmp))
  (require 'package nil t)
  (package-initialize))
(setopt package-install-upgrade-built-in t)
;;;; use-package.el
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (unless
      (require 'use-package nil t)
    (defmacro use-package (&rest args))))
(use-package bind-key)

;;; Custom
(setq custom-file (my:locate-user-emacs-file "custom.el"))
;; (load custom-file t)

;;;; Font
;; Fontset
;; * PlemolJP: https://github.com/yuru7/PlemolJP
;; * Cica: https://github.com/miiton/Cica
(when window-system
  (let* ((font '("PlemolJP Console NF" . 16))
         ;; (font '("Cica" . 16))
         (spec (font-spec :family (car font) :size (cdr font) :weight 'medium)))
    (set-face-attribute 'default nil :font spec)
    (set-fontset-font nil 'ascii spec nil 'append)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 spec nil 'append))
  (setq initial-frame-alist default-frame-alist))
;; nerd-icons
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  (nerd-icons-scale-factor 0.9))
(use-package nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
;;; Theme
;;;; Color Theme
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-org-config))
;;;; Modeline
;; doom-modeline
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-icon t)
  (doom-modeline-project-detection 'project)
  :hook (after-init . doom-modeline-mode)
  :config
  (my:enable-mode column-number-mode)
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info major-mode process vcs)))
;; hide-mode-line
(use-package hide-mode-line
  :ensure t
  ;; :hook ((neotree-mode imenu-list-minor-mode) . hide-mode-line-mode)
  )
;;;; Misc.
(my:unless! early-init-compat
  ;; disable features
  (setq make-backup-files nil
        auto-save-default nil
        auto-save-list-file-prefix (expand-file-name ".saves-" my:d:tmp))
  ;; common settings
  (setq system-time-locale "C")
  (setq-default tab-width 4)
  (setq history-delete-duplicates t)
  ;; disable menu-bar, tool-bar, scroll-bar
  (my:disable-mode menu-bar-mode)
  (my:disable-mode tool-bar-mode)
  (set-scroll-bar-mode nil)
  ;; cursor
  (push '(cursor-type . bar) default-frame-alist)
  ;; No startup screen
  (setq inhibit-startup-screen t)
  ;; Bell
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  ;; window size
  (when window-system
    (pcase (system-name)
      (_ (progn (push '(height . 56) default-frame-alist)
                (push '(width . 117) default-frame-alist))))
    (setq initial-frame-alist default-frame-alist)))
;; indent
(setq-default indent-tabs-mode nil)
;; truncate lines
(setq-default truncate-lines t)
(add-hook 'org-mode-hook (lambda () (setq-local truncate-lines nil)))
;; display line number
(my:if! (eval-and-compile (version<= "26.1" emacs-version))
    (progn
      (global-display-line-numbers-mode)
      (setq-default display-line-numbers-width 4))
  (progn
    (my:enable-mode global-linum-mode)
    (setq linum-format "%5d ")))
(custom-set-faces
 '(line-number ((t (:inherit default :slant normal :foreground "#6272a4" :weight normal))))
 '(line-number-current-line ((t (:inherit (hl-line default) :foreground "#f8f8f2" :slant normal :weight normal)))))
;; highlight line
(my:enable-mode global-hl-line-mode)
;; (add-hook
;;  'doc-view-mode-hook
;;  (lambda ()
;;    (progn
;;      (my:disable-mode linum-mode)
;;      (my:disable-mode hl-line-mode))))
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
(set-locale-environment "en_US.UTF-8")
;; (setq-default file-name-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
;; (set-selection-coding-system 'utf-8-unix)
;; (set-buffer-file-coding-system 'utf-8-unix)
;; (set-language-environment 'Japanese)
;; (setq-default buffer-file-coding-system 'utf-8-unix)
;;;; Misc.
;;  PATH -- exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :if (eval-and-compile
        (and window-system
             (not (getenv "WSL_INTEROP"))))
  :config
  (exec-path-from-shell-initialize))
;; transparent-mode
(use-package tp-mode
  :if (eval-and-compile
        (and window-system
             (or (not (getenv "WSL_INTEROP"))
                 (not (eq window-system 'x)))))
  :load-path "share"
  :commands (tp-mode)
  :custom
  (tp-mode-alpha 95)
  (tp-mode t))

;; Indent
(setq-default tab-width 4)
;; key binding
(bind-keys
 ("C-m" . newline-and-indent)
 ("C-h" . delete-backward-char)
 ("C-c g" . goto-line)) ; C-h -> Backspace
;; .el > .elc
(setq load-prefer-newer t)
;; scroll for one line
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ; for shell-mode

;; definition temporary files and shared files
(setq url-configuration-directory (expand-file-name "url" my:d:tmp)
      nsm-settings-file (expand-file-name "network-settings.data" my:d:tmp)
      project-list-file (expand-file-name "projects" my:d:tmp)
      bookmark-default-file (expand-file-name "bookmarks" my:d:share))
;; save minibuffer history
(add-hook 'after-init-hook (lambda () (my:enable-mode savehist-mode)))
(setq message-log-max 10000)
(setq history-length t
      savehist-file (expand-file-name "history" my:d:tmp))
;; yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)
;; not add newline at end of buffer
(setq next-line-add-newlines nil)
;; kill whole line when kill line
(setq kill-whole-line t)
;; global-auto-revert-mode
(add-hook 'after-init-hook (lambda () (my:enable-mode global-auto-revert-mode)))
;; time-stamp
(add-hook 'before-save-hook #'time-stamp)
;; tramp
(setq tramp-default-method "ssh"
      tramp-persistency-file-name (expand-file-name "tramp" my:d:tmp))
;; dired
;; (use-package dired-aux
;;   :defer t)
(setq dired-dwim-target t               ; default copy target in 2 windows
      dired-recursive-copies 'always    ; recursive directory copy
      dired-isearch-filenames t         ; only match filenames
      dired-listing-switches "-alhF")
;; generic-x
(use-package generic-x
  :defer t)
;; saveplace
(setq save-place-file (expand-file-name "save-places" my:d:tmp))
(add-hook 'after-init-hook #'save-place-mode)
;; uniquify
;; for same name buffer
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-min-dir-content 1)
;; Clipboard
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(use-package xclip
  :ensure t
  :if window-system
  :custom
  (xclip-method (or (and xclip-use-pbcopy&paste 'pbpaste)
					;; (and (executable-find "wl-copy") 'wl-copy)
					'xclip))
  :config
  (defun wl-copy (data) (xclip-set-selection 'clipboard data))
  (setq interprogram-cut-function 'wl-copy)
  (when (eval-and-compile (executable-find "wl-copy"))
    (defun wl-paste () (shell-command-to-string "wl-paste -n | tr -d \r"))
    (setq interprogram-paste-function 'wl-paste)))
;; popwin
(use-package popwin
  :ensure t
  :hook (after-init . popwin-mode)
  :config
  (add-to-list 'popwin:special-display-config '("*Compile-Log*"))
  (add-to-list 'popwin:special-display-config '("*Buffer List*"))
  (add-to-list 'popwin:special-display-config '("*Warnings*"))
  (add-to-list 'popwin:special-display-config '("*system-packages*"))
  (add-to-list 'popwin:special-display-config '("*Async Shell Command*")))
;; popup
;; (use-package popup
;;   :ensure t)
;; move window
(use-package windmove
  :bind (("M-<left>" . windmove-left)
         ("M-<right>" . windmove-right)
         ("M-<up>" . windmove-up)
         ("M-<down>" . windmove-down)))

;;; Global Packages
(use-package evil
  :ensure t
  :commands (evil-mode)
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

;;;; project.el
(use-package project-rootfile
  :ensure t
  :config
  (push #'project-rootfile-try-detect project-find-functions))
;;;; Mini-buffer completion
;; prescient -- simple but effective sorting and filtering for Emacs.
(use-package prescient
  :ensure t
  :hook (after-init . prescient-persist-mode)
  :custom
  (prescient-filter-method '(initialism literal prefix regexp))
  (prescient-use-char-folding t)
  (prescient-use-case-folding 'smart)
  (prescient-sort-full-matches-first t)
  (prescient-sort-length-enable nil)
  (prescient-aggressive-file-save t)
  (prescient-save-file (expand-file-name "prescient-save.el" my:d:tmp))
  (prescient-history-length 5000))
;; marginalia - Marginalia in the minibuffer
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-align 'right)
  (marginalia-align-offset (my:if! window-system 0 -1))
  :hook (after-init . marginalia-mode))
;; vertico -- VERTical Interactive COmpletion
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-count 30)
  (vertico-cycle t)
  (vertico-preselect 'first)
  (vertico-sort-function nil)
  ;; (vertico-sort-function #'vertico-sort-alpha)
  (vertico-count-format (cons "%-6s" (concat (nerd-icons-faicon "nf-fa-sort_amount_asc") " (%s/%s) ")))
  :config
  (my:enable-mode vertico-multiform-mode)
  ;; truncate long lines
  (use-package vertico-truncate
    :load-path "share"
    :hook (vertico-mode . vertico-truncate-mode))
  ;; Prefix current candidate with arrow
  (defvar +vertico-current-arrow t)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                   (not (bound-and-true-p vertico-flat-mode)))
                                              (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (if (bound-and-true-p vertico-grid-mode)
        (if (= vertico--index index)
            (concat #("> " 0 1 (face vertico-current)) cand)
          (concat #("_" 0 1 (display " ")) cand))
      (if (= vertico--index index)
          (concat
           #("> " 0 1 (face vertico-current))
           cand)
        (concat "  " cand))))

  (defvar +vertico-transform-functions nil)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))
  ;; function to highlight enabled modes similar to counsel-M-x
  (defun +vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it as font-lock-constant-face."
    (let ((sym (intern cmd)))
      (if (or (eq sym major-mode)
              (and
               (memq sym minor-mode-list)
               (boundp sym)))
          (propertize cmd 'face 'font-lock-constant-face)
        cmd)))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 (+vertico-transform-functions . +vertico-highlight-enabled-mode)))
  (defun my:find-file-with-alpha-sort ()
    "Temporarily set `vertico-sort-function` to `#'vertico-sort-alpha` and run `find-file`."
    (interactive)
    (let ((vertico-sort-function #'vertico-sort-alpha))
      (call-interactively #'find-file)))
  (bind-key "C-x M-f" #'my:find-file-with-alpha-sort))
;; vertico + prescient
(use-package vertico-prescient
  :ensure t
  :hook (prescient-persist-mode . vertico-prescient-mode))
;; consult - Consulting completing-read
(use-package consult
  :ensure t
  :bind (("C-x C-r" . consult-recent-file)
         ("C-x b" . consult-buffer)
         ("M-y" . yank-pop)
         ("M-i" . consult-line)
         ("M-r" . consult-ripgrep)
         :map isearch-mode-map
         ("M-i" . consult-line)))
(use-package consult-project-extra
  :ensure t
  :bind (("M-p" . consult-project-extra-find)))
(use-package consult-ghq
  :ensure t
  :bind (("M-g" . consult-ghq-switch-project))
  :custom
  (project-switch-commands 'project-find-file))
;; embark -- Emacs Mini-Buffer Actions Rooted in Keymaps
(use-package embark
  :ensure t
  :bind
  (("M-a" . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("<f1> B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (use-package embark-consult
    :ensure t))

;;;; avy, ace
;; avy
(use-package avy
  :ensure t
  :bind (("C-^" . avy-goto-char-timer)
         ("C-]" . avy-goto-line)))
;; ace-window
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?h ?j ?k ?l ?u ?i ?o ?p))
  :custom-face
  (aw-leading-char-face ((t (:height 2.0 :forground "#f1fa8c")))))

;;;; corfu -- in-buffer completion
;; (use-package completion-preview-mode
;;   :bind
;;   (:map completion-preview-active-mode-map
;;         ("C-n" . completion-preview-next-candidate)
;;         ("C-p" . completion-preview-prev-candidate))
;;   :custom
;;   (completion-preview-minimum-symbol-length 1)
;;   :hook (after-init . (lambda () (my:enable-mode global-completion-preview-mode)))
;;   )
(use-package corfu
  :ensure t
  :hook (after-init . (lambda () (global-corfu-mode) (my:enable-mode corfu-popupinfo-mode)))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary nil)
  (corfu-preselect 'valid)
  (corfu-left-margin-width 1.0)
  (corfu-right-margin-width 1.0)
  (corfu-bar-width 0.5)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.1)
  (corfu-quit-no-match t)
  (corfu-popupinfo-delay '(0.1 . 0.1))
  (corfu-popupinfo-resize nil)
  :config
  (use-package nerd-icons-corfu
    :ensure t
    :custom
    (nerd-icons-corfu-mapping
     '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
       (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
       ;; (class :style "cod" :icon "symbol_class" :face font-lock-type-face)
       (class :style "fa" :icon "cogs" :face font-lock-type-face)
       (color :style "cod" :icon "symbol_color" :face success)
       (command :style "cod" :icon "terminal" :face default)
       (constant :style "cod" :icon "symbol_constant" :face font-lock-constant-face)
       (constructor :style "cod" :icon "triangle_right" :face font-lock-function-name-face)
       (enummember :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
       (enum-member :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
       (enum :style "cod" :icon "symbol_enum" :face font-lock-builtin-face)
       (event :style "cod" :icon "symbol_event" :face font-lock-warning-face)
       (field :style "cod" :icon "symbol_field" :face font-lock-variable-name-face)
       (file :style "cod" :icon "symbol_file" :face font-lock-string-face)
       (folder :style "cod" :icon "folder" :face font-lock-doc-face)
       (interface :style "cod" :icon "symbol_interface" :face font-lock-type-face)
       ;; (keyword :style "cod" :icon "symbol_keyword" :face font-lock-keyword-face)
       (keyword :style "fa" :icon "key" :face font-lock-keyword-face)
       (macro :style "cod" :icon "symbol_misc" :face font-lock-keyword-face)
       (magic :style "cod" :icon "wand" :face font-lock-builtin-face)
       (method :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
       ;; (function :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
       (function :style "fa" :icon "cube" :face font-lock-function-name-face)
       ;; (module :style "cod" :icon "file_submodule" :face font-lock-preprocessor-face)
       (module :style "fa" :icon "code" :face font-lock-preprocessor-face)
       (numeric :style "cod" :icon "symbol_numeric" :face font-lock-builtin-face)
       (operator :style "cod" :icon "symbol_operator" :face font-lock-comment-delimiter-face)
       (param :style "cod" :icon "symbol_parameter" :face default)
       (property :style "cod" :icon "symbol_property" :face font-lock-variable-name-face)
       (reference :style "cod" :icon "references" :face font-lock-variable-name-face)
       (snippet :style "cod" :icon "symbol_snippet" :face font-lock-string-face)
       (string :style "cod" :icon "symbol_string" :face font-lock-string-face)
       (struct :style "cod" :icon "symbol_structure" :face font-lock-variable-name-face)
       (text :style "cod" :icon "text_size" :face font-lock-doc-face)
       (typeparameter :style "cod" :icon "list_unordered" :face font-lock-type-face)
       (type-parameter :style "cod" :icon "list_unordered" :face font-lock-type-face)
       (unit :style "cod" :icon "symbol_ruler" :face font-lock-constant-face)
       (value :style "cod" :icon "symbol_field" :face font-lock-builtin-face)
       ;; (variable :style "cod" :icon "symbol_variable" :face font-lock-variable-name-face)
       (variable :style "fa" :icon "tag" :face font-lock-variable-name-face)
       ;; (t :style "cod" :icon "code" :face font-lock-warning-face)
       (t :style "fa" :icon "code" :face font-lock-warning-face)
       ))
    :config
    (push #'nerd-icons-corfu-formatter corfu-margin-formatters))
  (use-package corfu-terminal
    :ensure t
    :if (not window-system)
    :hook (global-corfu-mode . corfu-terminal-mode))
  ;;;;; orderless
  (use-package orderless
    :ensure t
    :custom
    (completion-styles '(orderless-fast orderless-flex basic))
    (completion-category-overrides '((file (styles basic partial-completion))))
    ;; (orderless-matching-styles '(orderless-literal orderless-flex))
	:config
	(defun orderless-fast-dispatch (word index total)
	  (and (= index 0) (= total 1) (length< word 5)
		   `(orderless-regexp . ,(concat "^" (regexp-quote word)))))
	(orderless-define-completion-style orderless-fast
	  (orderless-style-dispatchers '(orderless-fast-dispatch))
	  (orderless-matching-styles '(orderless-literal orderless-regexp)))
	(orderless-define-completion-style orderless-flex
	  (orderless-style-dispatchers '(orderless-affix-dispatch))
	  (orderless-matching-styles '(orderless-literal orderless-flex))))
  (use-package corfu-prescient
    :ensure t
    :hook (global-corfu-mode . corfu-prescient-mode)
    :config
    (with-eval-after-load 'orderless
      (setq corfu-prescient-enable-filtering nil))))
;;;; cape -- for corfu
(use-package cape
  :ensure t
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

;;;; Tab
;; Tab-bar-mode or Elscreen
(use-package tab-bar
  ;;:if (eval-when-compile (version<= "27" emacs-version))
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-name-function
   (lambda () (concat "ǀ " (tab-bar-tab-name-current-with-count))))
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
;; (use-package centaur-tabs
;;   :ensure t
;;   :hook (after-init . centaur-tabs-mode)
;;   :custom
;;   (centaur-tabs-prefix-key (kbd "M-z"))
;;   (centaur-tabs-style "bar")
;;   (centaur-tabs-height 12)
;;   (centaur-tabs-set-icons t)
;;   (centaur-tabs-set-bar 'left)
;;   (centaur-tabs-set-modified-marker t)
;;   (centaur-tabs-cycle-scope 'tabs)
;;   :config
;;   ;; (centaur-tabs-group-by-projectile-project)
;;   (bind-keys
;;    ("C-<tab>" . centaur-tabs-forward)
;;    ("<C-S-iso-lefttab>" . centaur-tabs-backward)
;;    :prefix-map centaur-tabs-prefix-map
;;    :prefix "M-z"
;;    ("n" . centaur-tabs-forward)
;;    ("C-n" . centaur-tabs-forward)
;;    ("M-n" . centaur-tabs-forward)
;;    ("p" . centaur-tabs-backward)
;;    ("C-p" . centaur-tabs-backward)
;;    ("M-p" . centaur-tabs-backward)
;;    ("k" . kill-current-buffer)
;;    ("C-k" . kill-current-buffer)
;;    ("M-k" . kill-current-buffer)
;;    ("f" . centaur-tabs-forward-group)
;;    ("C-f" . centaur-tabs-forward-group)
;;    ("M-f" . centaur-tabs-forward-group)
;;    ("b" . centaur-tabs-backward-group)
;;    ("C-b" . centaur-tabs-backward-group)
;;    ("M-b" . centaur-tabs-backward-group)
;;    ("C-a" . centaur-tabs-select-beg-tab)
;;    ("C-e" . centaur-tabs-select-end-tab)))
;; eldoc
(use-package eldoc-box
  :ensure t
  :if window-system
  :hook
  (eglot-managed-mode . eldoc-box-hover-mode)
  (lspce-mode . eldoc-box-hover-mode)
  ;; (lsp-proxy-mode . eldoc-box-hover-mode)
  ;; ((eldoc-box-hover-mode) . centaur-tabs-local-mode)
  :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :custom
  (eldoc-box-lighter t)
  (eldoc-box-max-pixel-width 500)
  (eldoc-box-max-pixel-height 400)
  (eldoc-box-only-multi-line (not window-system))
  (eldoc-box-offset '(16 24 16))
  (eldoc-box-clear-with-C-g t)
  :custom-face
  (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
  (eldoc-box-body ((t (:inherit tooltip))))
  :config
  ;; Always retrun 'left so that eldoc-box is aligned to right.
  ;; (defun eldoc-box--window-side () 'left)
  (setq eldoc-idle-delay 0.1)
  (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
        (alist-get 'right-fringe eldoc-box-frame-parameters) 8))
;;; Text
;;;; migemo
(defvar migemo-p (eval-and-compile (executable-find "cmigemo")))
(use-package migemo
  :ensure t
  :if migemo-p
  :init
  (add-hook 'after-init-hook #'migemo-init)
  :custom
  (migemo-coding-system 'utf-8-unix)
  (migemo-command "cmigemo")
  (migemo-dictionary
   (eval-and-compile
     (or (my:path-exists? "/usr/share/cmigemo/utf-8/migemo-dict")
         (my:path-exists? "/usr/share/migemo/migemo-dict"))))
  (migemo-regex-dictionary nil)
  (migemo-user-dictionary nil)
  (migemo-isearch-min-length 1)
  (migemo-options '("-q" "--emacs"))
  (migemo-isearch-enable-p nil))
;;;; IME -- ddskk
(use-package skk
  :ensure ddskk
  :bind (("C-\\" . skk-mode))
  :config
  (defvar my:d:skk (expand-file-name "app/SKK" my:d:nextcloud))
  (setq skk-large-jisyo (expand-file-name "SKK-JISYO.L+emoji.utf8" my:d:skk))
  (setq skk-user-directory my:d:skk
        skk-jisyo (expand-file-name "jisyo.utf8" my:d:skk)
        skk-backup-jisyo (expand-file-name "SKK/jisyo.bak" my:d:tmp)
        skk-study-backup-file (expand-file-name "SKK/study.bak" my:d:tmp))
  (setq skk-jisyo-code 'utf-8)
  (setq default-input-method "japanese-skk")
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
        (lambda (word &optional purge)
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
;; flyspell
(use-package flyspell
  :hook ((org-mode text-mode LaTeX-mode) . flyspell-mode)
  :config
  (setq ispell-program-name "hunspell")
  (defvar flyspell-correct-map (make-sparse-keymap))
  (defvar ispell-regexp-ja "[一-龠ぁ-🈀ァ-𛀀ー・、。々]+"
    "Regular expression to match a Japanese word.
The expression can be [^\000-\377]+, [^!-~]+, or [一-龠ぁ-🈀ァ-𛀀ー・、。々]+")
  ;; (add-to-list 'ispell-skip-region-alist (list ispell-regexp-ja))
  (defun flyspell-skip-ja (beg end info)
    "Tell flyspell to skip a Japanese word.
Call this on `flyspell-incorrect-hook'."
    (string-match ispell-regexp-ja (buffer-substring beg end)))
  (add-hook 'flyspell-incorrect-hook 'flyspell-skip-ja)
  (use-package flyspell-correct-avy-menu
    :ensure t
    :init
    (bind-keys :prefix-map flyspell-correct-map
               :prefix "C-c"
               ("s" . flyspell-correct-wrapper))))
;;;; Translater
(use-package google-translate
  :ensure t
  :commands (google-translate-at-point google-translate-at-point-reverse)
  :init
  (bind-keys
   :prefix-map google-translate-map
   :prefix "M-t"
   ("M-t" . google-translate-at-point)
   ("M-r" . google-translate-at-point-reverse))
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja"))

;;; Common Packages
;;;; eww
(use-package eww
  :preface
  (setq my:d:eww (expand-file-name "eww" my:d:tmp))
  :commands (eww)
  :custom
  (eww-bookmarks-directory my:d:eww)
  (eww-history-limit 9999)
  (eww-search-prefix "https://www.google.com/search?q="))
;;;; undo
;; undohist
(use-package undohist
  :ensure t
  :hook (after-init . undohist-initialize)
  :custom
  (undohist-directory (expand-file-name "undohist" my:d:tmp))
  :config
  (defun undohist--auto-recover (orig-fun &rest args)
    "Ignore yes/no prompt in `undohist-recover-1'."
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
              ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
      (apply orig-fun args)))
  (advice-add 'undohist-recover-1 :around #'undohist--auto-recover))
;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undohist" my:d:tmp))))
  (undo-tree-visualizer-timestamps t))
;;;; yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :ensure t
    :defer t)
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" my:d:share)))

;;; VCS -- Git
(setq find-file-visit-truename t)
(setq vc-follow-symlinks t)
(use-package diff-hl
  :ensure t
  :hook (after-init . global-diff-hl-mode))
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(use-package git-modes
  :ensure t
  :mode ((".gitconfig" . gitconfig-mode)
         (".gitignore" . gitignore-mode)
         (".gitattributes" . gitattributes-mode)))

;;; Programming Language
(defun git-rsync-push ()
  (interactive)
  (if (executable-find "git-rsync")
      (call-process-shell-command "git rsync push &")
    (message "git-rsync not found.")))
(defvar auto-git-rsync nil)
(defun toggle-auto-git-rsync ()
  (interactive)
  (if auto-git-rsync
      (progn
        (remove-hook 'after-save-hook #'git-rsync-push nil)
        (setq auto-git-rsync nil)
        (message "auto-git-rsync is disabled."))
    (progn
      (add-hook 'after-save-hook #'git-rsync-push nil nil)
      (setq auto-git-rsync t)
      (message "auto-git-rsync is enabled."))))

;;;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("<f8>"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :custom
  (treemacs-sorting 'alphabetic-asc)
  (treemacs-file-follow-delay 0.05)
  (treemacs-file-event-delay 50)
  (treemacs-tag-follow-delay 0.05)
  (treemacs-deferred-git-apply-delay 0.05)
  (treemacs-litter-directories
   '("/node_modules"
     "/.venv"
     "/.cask"
     "/__pycache__"
     "/.pytest_cache"
     "/.cache"))
  (treemacs-persist-file (expand-file-name "treemacs-persist" my:d:tmp))
  (treemacs-last-error-persist-file (expand-file-name "treemacs-persist-at-last-error" my:d:tmp))
  :config
  (setq treemacs--icon-size 14)
  (my:enable-mode treemacs-project-follow-mode)
  (my:enable-mode treemacs-follow-mode)
  (my:enable-mode treemacs-filewatch-mode))
(use-package project-treemacs
  :ensure t
  :defer t
  :after (treemacs))

;;;; flycheck
;; (use-package flycheck
;;   :ensure t
;;   :hook ((python-base-mode) . flycheck-mode)
;;   :config
;;   (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;;;; LSP
(use-package jsonrpc
  :defer t
  :config
  (setq jsonrpc-default-request-timeout 3000)
  (fset #'jsonrpc--log-event #'ignore))
;; LSPCE
(use-package lspce
  :load-path "share/lspce"
  :bind (("M-/" . xref-find-references)
         :prefix-map lspce-mode-map
         :prefix "M-e"
         ("M-e r" . lspce-rename)
		 ("M-e e". lspce-restart-server))
  :hook
  ((sh-base-mode c++-ts-mode rust-ts-mode rust-mode go-ts-mode lua-ts-mode tex-mode yatex-mode) . lspce-mode)
  ((python-mode python-ts-mode) .
   (lambda ()
     (pyvenv-deactivate)
     (my:enable-mode pet-mode)
     (when python-shell-virtualenv-root
       (pyvenv-activate python-shell-virtualenv-root))
     (message buffer-file-name)
     (my:enable-mode lspce-mode)))
  :custom
  (lspce-enable-symbol-highlight t)
  (lspce-send-changes-idle-time 0.1)
  (lspce-idle-delay 0.05)
  (lspce-show-log-level-in-modeline nil)
  (lspce-modes-enable-single-file-root '(python-mode python-ts-mode sh-base-mode lua-ts-mode))
  :config
  (lspce-change-max-diagnostics-count 10000)
  (defun lspce-eldoc-function (callback)
    (when lspce-mode
      (let ((hover-info (and lspce-eldoc-enable-hover (lspce--hover-at-point)))
            (signature (and lspce-eldoc-enable-signature (lspce--signature-at-point)))
            backend
            content
            document)
        (when hover-info
          (setq content (lspce--eldoc-render-markup (nth 1 hover-info))))
        (cond
         ((and signature content)
          (setq document (concat signature "\n\n" content)))
         ((or signature content)
          (setq document (concat signature content))))
        (when document
          (setq backend (propertize "" 'face 'lspce-eldoc-backend-face))
          (funcall callback (concat backend document))))))

  ;; You should call this first if you want lspce to write logs
  (lspce-set-log-file "/tmp/lspce.log")

  ;; By default, lspce will not write log out to anywhere. 
  ;; To enable logging, you can add the following line
  ;; (lspce-enable-logging)
  ;; You can enable/disable logging on the fly by calling `lspce-enable-logging' or `lspce-disable-logging'.

  ;; modify `lspce-server-programs' to add or change a lsp server, see document
  ;; of `lspce-lsp-type-function' to understand how to get buffer's lsp type.
  ;; Bellow is what I use
  (setq lspce-server-programs
        `(("rust"  "rust-analyzer" "" lspce-ra-initializationOptions)
          ("python" "basedpyright-langserver" "--stdio")
          ("sh" "bash-language-server" "start")
          ;; ("C" "clangd" "--all-scopes-completion --clang-tidy --enable-config --header-insertion-decorators=0")
          ;; ("java" "java" lspce-jdtls-cmd-args lspce-jdtls-initializationOptions)
          ))
  )

;; lsp-proxy
;; (use-package lsp-proxy
;;   :load-path "share"
;;   :bind (("M-/" . xref-find-references)
;;          :prefix-map lsp-proxy-mode-map
;;          :prefix "M-e"
;;          ("M-e r" . lsp-proxy-rename)
;;  		 ("M-e e". lsp-proxy-restart))
;;   :hook
;;   ((sh-base-mode c++-ts-mode rust-ts-mode rust-mode go-ts-mode lua-ts-mode tex-mode yatex-mode) . lsp-proxy-mode)
;;   ((python-mode python-ts-mode) .
;;    (lambda ()
;;      (pyvenv-deactivate)
;;      (my:enable-mode pet-mode)
;;      (when python-shell-virtualenv-root
;;        (pyvenv-activate python-shell-virtualenv-root))
;;      (my:enable-mode lsp-proxy-mode)))
;;   :custom
;;   (lsp-proxy-idle-delay 0.05)
;; )

;; eglot
;; (use-package eglot
;;   :ensure t
;;   :pin gnu
;;   :defer t
;;   :bind (("M-/" . xref-find-references)
;;          :prefix-map eglot-mode-map
;;          :prefix "M-e"
;;          ("M-e r" . eglot-rename)
;; 		 ("M-e e". eglot))
;;   :hook
;;   ((sh-base-mode c++-ts-mode rust-ts-mode rust-mode go-ts-mode lua-ts-mode tex-mode yatex-mode) . eglot-ensure)
;;   ((python-mode python-ts-mode) .
;;    (lambda ()
;;      (pyvenv-deactivate)
;;      (my:enable-mode pet-mode)
;;      (when python-shell-virtualenv-root
;;        (pyvenv-activate python-shell-virtualenv-root))
;;      (eglot-ensure)))
;;   :custom
;;   (eglot-events-buffer-config 0)
;;   (eglot-autoshutdown t)
;;   (eglot-autoreconnect t)
;;   ;; (eglot-ignored-server-capabilities '(:documentHighlightProvider))
;;   (eglot-ignored-server-capabilities '(:didChangeWatchedFiles))
;;   :init
;;   (setq eglot-ignored-server-capabilities '(:didChangeWatchedFiles :reportUnusedCallResult))
;;   (setq-default
;;     eglot-workspace-configuration
;;     '(:basedpyright\.analysis
;;        (:inlayHints
;;          (:variableTypes nil
;;           :callArgumentNames nil
;;           :functionReturnTypes nil
;;           :genericTypes nil
;;          )
;;         :diagnosticSeverityOverrides
;;          (:reportUnusedCallResult "none"
;;           :reportMissingTypeStubs "none"
;;           :reportImplicitOverride "none"
;;          )
;;        )))
;;   :config
;;   (use-package eglot-booster
;;     :after eglot
;;     ;; :vc (:fetcher github :repo "jdtsmith/eglot-booster")
;;     :load-path "share"
;;     :custom
;;     (eglot-booster-io-only t)
;;     :config
;;     (eglot-booster-mode))
;;   (add-to-list
;;    'eglot-server-programs
;;    '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio"))
;;    ;; '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))
;;    ;; '((python-mode python-ts-mode) . ("pylsp"))
;;    ;; '((python-mode python-ts-mode) . ("ruff" "server" "--preview"))
;;    ))

;; tree-sitter
(use-package treesit
  :custom
  (treesit-font-lock-level 3))
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install t)
  :config
  (delete 'python treesit-auto-langs)
  (delete 'bash treesit-auto-langs)
  (delete 'rust treesit-auto-langs)
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist))

;;;; C, C++
;; (use-package ccls
;;   :if (executable-find "ccls")
;;   :ensure t
;;   :config
;;   (setq ccls-executable (executable-find "ccls")))
;; (setq treesit-language-source-alist
;;       '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;         (c "https://github.com/tree-sitter/tree-sitter-c")))
;; (setq treesit-load-name-override-list
;;       '((c++ "libtree-sitter-cpp")))
;; (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
(push (cons "\\.cu$" 'c++-ts-mode) auto-mode-alist)
(push (cons "\\.cuh$" 'c++-ts-mode) auto-mode-alist)

;;;; python
;; python-mode
(use-package python
  :defer t
  :config
  (use-package pet
    :ensure t)
  (use-package pyvenv
    :ensure t)
  ;; python-insert-docstring: for google-style docstring
  (use-package python-insert-docstring
    :ensure t
    :bind (:map python-mode-map
                ("C-c i" . python-insert-docstring-with-google-style-at-point)
                :map python-ts-mode-map
                ("C-c i" . python-insert-docstring-with-google-style-at-point)))
  ;; ruff
  (use-package reformatter
    :ensure t
    :config
    (reformatter-define ruff-format
      :program "ruff"
      :args (list "format" "--stdin-filename" (or (buffer-file-name) input-file))
      :lighter " RuffFmt")
    (reformatter-define ruff-check
      :program "ruff"
      :args (list "check" "--fix" "--unsafe-fixes" "--extend-select" "I" "--stdin-filename" (or (buffer-file-name) input-file))
      :lighter " RuffCheck")
    (defun ruff-fmt ()
      (interactive)
      (ruff-check-buffer)
      (ruff-format-buffer)
      (message "Formatted."))
    (bind-keys
     :map python-mode-map
     ("C-c C-f" . ruff-fmt)
     :map python-ts-mode-map
     ("C-c C-f" . ruff-fmt))))
;;;; bison, flex
(use-package bison-mode
  :ensure t
  :mode
  (("\.\(y\|yy\)$" . bison-mode)
   ("\.\(l\|ll\)$" . flex-mode)))

;;;; Rust
(use-package rust-mode
  :mode (("\\.rs$" . rust-mode))
  :ensure t
  :defer t
  :custom
  (rust-format-on-save t)
  (rust-mode-treesitter-derive t)
  :config
  (use-package cargo
    :ensure t
    :hook (rust-mode . cargo-minor-mode))
  )

;;;; Golang
(defun go-mode-omnibus ()
  ;; Go code formatting by goimports
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))
;;(add-hook 'go-mode-hook 'go-mode-omnibus)
(add-hook 'go-ts-mode-hook 'go-mode-omnibus)
;;;; Lua
(use-package lua-mode
  :ensure t
  :mode (("\\.lua$" . lua-mode)))

;;;; shell script
(use-package fish-mode
  :ensure t
  :mode (("\\.fish$" . fish-mode)))
;; (use-package ebuild-mode
;;   :if my:gentoo-p
;;   :mode
;;   (("make.conf" . sh-ts-mode)))

;;;; YAML
(use-package yaml-mode
  :ensure t
  :mode
  (("\\.yaml$" . yaml-mode)
   ("\\.yml$" . yaml-mode))
  :custom
  (yaml-indent-offset 4))

;;;; TeX
(use-package yatex
  :ensure t
  :mode
  (("\\.tex$" . yatex-mode)
   ("\\.sty$" . yatex-mode)
   ("\\.cls$" . yatex-mode)
   ("\\.ltx$" . yatex-mode)
   ("\\.bbl$" . yatex-mode))
  :init
  (add-hook 'yatex-mode-hook 'turn-on-reftex)
  (add-hook 'yatex-mode-hook 'display-line-numbers-mode)
  (add-hook 'yatex-mode-hook #'(lambda () (setq-local truncate-lines nil)))
  :config
  ;; (setq tex-command "latexmk -c && latexmk -f")
  ;; (add-to-list 'display-buffer-alist
  ;;              (cons "\\*YaTeX-typesetting\\*.*" (cons #'display-buffer-no-window nil)))
  (setq tex-command "latexmk -f")
  (setq YaTeX-kanji-code 4)
  (setq YaTeX-environment-indent 2)
  (setq tex-pdfview-command (executable-find "okular"))
  (setq electric-indent-mode nil)
  (setq reftex-use-external-file-finders t))
;; PDF
(use-package pdf-tools
  :ensure t
  :if window-system
  :mode
  (("\\.pdf$" . pdf-view-mode))
  :custom
  (pdf-view-resize-factor 1.0)
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (my:disable-mode display-line-numbers-mode)))
  (bind-keys :map pdf-view-mode-map ("C-s" . isearch-forward)))

;;;; Lisp -- Emacs Lisp, Common Lisp, Scheme
;;;;; Emacs Lisp
(defun compile-init-el ()
  "Compile init.el."
  (interactive)
  (async-shell-command
   (mapconcat #'shell-quote-argument
              `("make" "-C" ,user-emacs-directory "build") " ")))
(bind-key "C-c C-c" #'compile-init-el emacs-lisp-mode-map)
;;;;; Common Lisp
;; SLIME
;; (defvar quicklisp-directory (eval-when-compile (my:path-exists? (my:locate-home "quicklisp"))))
;; (use-package slime
;;   :if quicklisp-directory
;;   :commands (slime)
;;   :ensure t
;;   ;; :ensure slime-company
;;   :config
;;   (setq inferior-lisp-program "sbcl")
;;   (defun lisp-hook ()
;;     (load (expand-file-name "slime-helper.el" quicklisp-directory))
;;     ;; (slime-setup '(slime-fancy slime-company)))
;;     (slime-setup '(slime-fancy)))
;;   (add-hook 'lisp-mode-hook #'lisp-hook))
;;;;; Scheme
(eval-and-compile
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
    (bind-key "C-c S" 'scheme-other-window)))

;;;; Web
(use-package web-mode
  :ensure t
  :hook ((sgml-mode css-mode html-mode) . web-mode))

;;;; Ruby
;; enh-ruby-mode
;; (use-package enh-ruby-mode
;;   :ensure t
;;   :if (executable-find "ruby")
;;   :mode
;;   (("\\.rb$" . enh-ruby-mode)
;;    ("\\.rake$" . enh-ruby-mode)
;;    ("\\.cap$" . enh-ruby-mode)
;;    ("config.ru$" . enh-ruby-mode)
;;    ("Rakefile$" . enh-ruby-mode)
;;    ("Capfile$" . enh-ruby-mode)
;;    ("Gemfile$" . enh-ruby-mode))
;;   :config
;;   (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
;;   (defun my:ruby-mode-hook-function ()
;;     (setq enh-ruby-deep-indent-paren nil)
;;     (setq enh-ruby-deep-indent-paren-style nil)
;;     (setq enh-ruby-use-encoding-map nil)
;;     (lambda () (ruby-electric-mode t))
;;     (setq ruby-electric-expand-delimiters-list nil)
;;     (lambda () (ruby-block-mode t)
;;       (setq ruby-block-highlight-toggle t)))
;;   (add-hook 'enh-ruby-mode-hook 'my:ruby-mode-hook-function)
;;   ;; 保存時にmagic commentを追加しないようにする
;;   (defadvice enh-ruby-mode-set-encoding (around stop-enh-ruby-mode-set-encoding)
;;     "If enh-ruby-not-insert-magic-comment is true, stops enh-ruby-mode-set-encoding."
;;     (if (and (boundp 'enh-ruby-not-insert-magic-comment)
;;              (not enh-ruby-not-insert-magic-comment))
;;         ad-do-it))
;;   (ad-activate 'enh-ruby-mode-set-encoding)
;;   (setq-default enh-ruby-not-insert-magic-comment t))
;; ;; robe
;; (use-package robe
;;   :ensure t
;;   :ensure-system-package (pry . "gem install pry")
;;   :if (executable-find "pry")
;;   :hook ((enh-ruby-mode . robe-mode)))
;; ;; rubocop, ruby-lint
;; (use-package rubocop
;;   :ensure t
;;   :ensure-system-package ((rubocop . "gem install rubocop")
;;                           (ruby-lint . "gem install ruby-lint"))
;;   :if (executable-find "rubocop")
;;   :hook (enh-ruby-mode . rubocop-mode)
;;   :config
;;   ;; definition for flycheck
;;   (flycheck-define-checker ruby-rubylint
;;     "A Ruby syntax and style checker using the rubylint tool."
;;     :command ("ruby-lint" source)
;;     :error-patterns
;;     ((warning line-start
;;               (file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
;;               line-end)
;;      (error line-start
;;             (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
;;             line-end))
;;     :modes (enh-ruby-mode ruby-mode))
;;   (flycheck-define-checker ruby-rubocop
;;     "A Ruby syntax and style checker using the RuboCop tool."
;;     :command ("rubocop" "--format" "emacs" "--silent"
;;               (config-file "--config" flycheck-rubocoprc)
;;               source)
;;     :error-patterns
;;     ((warning line-start
;;               (file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
;;               line-end)
;;      (error line-start
;;    	        (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
;;             line-end))
;;     :modes (enh-ruby-mode motion-mode)))
;; ;; inf-ruby
;; (use-package inf-ruby
;;   :defer t
;;   :hook (enh-ruby-mode . inf-ruby-minor-mode))

;;;; outline-(minor-)?mode
(use-package outline
  :defer t
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
  :disabled t
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
         ("C-c t" . org-todo)
         ("C-c C-b" . org-beamer-export-to-pdf))
  :hook (org-mode . turn-on-font-lock)
  :init
  (setq org-directory (expand-file-name "org/" my:d:nextcloud))
  :custom
  (org-latex-pdf-process '("latexmk %f"))
  (org-export-in-background nil)
  (org-export-async-debug t)
  :config
  ;; (use-package org-install)
  (use-package org-capture)
  (use-package org-protocol)
  (use-package ox)
  (use-package ox-latex)
  (use-package ox-beamer)
  (use-package ox-gfm
    :ensure t)
  (use-package ox-rst
    :ensure t)
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("framesep=2mm")
          ("linenos=true")
          ("baselinestretch=1.2")
          ("fontsize=\\scriptsize")
          ("breaklines")
          ))
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
  ;; (use-package request
  ;;   :ensure t
  ;;   :init
  ;;   (setq request-storage-directory (expand-file-name "request" my:d:tmp))
  ;;   (unless (file-directory-p request-storage-directory)
  ;;     (make-directory request-storage-directory)))
  ;; (use-package org-gcal
  ;;   :ensure t
  ;;   :if (file-directory-p org-directory)
  ;;   :commands (org-gcal-fetch org-gcal-sync)
  ;;   :custom
  ;;   (org-generic-id-locations-file (expand-file-name "org-generic-id-locations" my:d:tmp))
  ;;   :init
  ;;   (setq org-gcal-dir (expand-file-name "org-gcal" my:d:tmp))
  ;;   (unless org-gcal-dir
  ;;     (make-directory org-gcal-dir))
  ;;   (setq org-gcal-token-file (expand-file-name ".org-gcal-token" org-gcal-dir))
  ;;   (setq alert-log-messages t)
  ;;   (setq alert-default-style 'log)
  ;;   (setq org-gcal-down-days   90) ;; 過去 3 month
  ;;   (setq org-gcal-up-days    180) ;; 未来 6 month
  ;;   (setq org-gcal-auto-archive nil)
  ;;   :config
  ;;   (load (expand-file-name "app/org-gcal/token" my:d:nextcloud))
  ;;   (setq org-gcal-file-alist `(("de9uch1@gmail.com" . ,(expand-file-name "schedule.org" org-directory)))))
  )

;;; Misc Packages
;;;; SSH
;; ssh-config-mode
(use-package ssh-config-mode
  :ensure t
  :defer t
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
  ;; Suppress "Cleaning up the recentf...done (0 removed)"
  (advice-add 'recentf-cleanup :around #'suppress-messages)
  (run-with-idle-timer 30 t (lambda () (let ((save-silently t)) (recentf-save-list))))

  ;; From recentf-ext
  ;;; `recentf' as most recently USED files
  (defun recentf-push-buffers-in-frame ()
    (walk-windows
     (lambda (win)
       (let ((bfn (buffer-local-value 'buffer-file-name (window-buffer win))))
         (and bfn (recentf-add-file bfn))))))
  (add-to-list 'window-configuration-change-hook 'recentf-push-buffers-in-frame)
  ;;; `recentf' directory
  (defun recentf-add-dired-directory ()
    (when (and (stringp dired-directory)
               (equal "" (file-name-nondirectory dired-directory)))
      (recentf-add-file dired-directory)))
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory))
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
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-enabled t))
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
;;;; vterm
(use-package vterm
  :commands (vterm)
  :defer t
  :ensure t
  :custom
  (vterm-max-scrollback 10000000)
  (vterm-buffer-name-string "vterm: %s")
  (vterm-keymap-exceptions '("<f1>" "<f12>" "C-c" "C-x" "C-u" "C-g" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y"))
  :config
  ;; Workaround of not working counsel-yank-pop
  ;; https://github.com/akermu/emacs-libvterm#counsel-yank-pop-doesnt-work
  ;; (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
  ;;   (if (equal major-mode 'vterm-mode)
  ;;       (let ((inhibit-read-only t)
  ;;             (yank-undo-function (lambda (_start _end) (vterm-undo))))
  ;;         (cl-letf (((symbol-function 'insert-for-yank)
  ;;                    (lambda (str) (vterm-send-string str t))))
  ;;           (apply orig-fun args)))
  ;;     (apply orig-fun args)))
  ;; (advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action))
  )
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
