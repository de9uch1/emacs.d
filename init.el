;;; init.el --- Emacs Initialization File -*- lexical-binding: t -*-

;; Filename: init.el
;; Description: my emacs configuration 
;; Package-Requires: ((emacs "26.1"))
;; Author: Hiroyuki Deguchi <deguchi@ai.cs.ehime-u.ac.jp>
;; Created: 2018-05-26
;; Modified: 2020-07-07
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
;;;; Tuning and Speed Up:
(setq gc-cons-percentage 0.75)
(setq gc-cons-threshold (* 512 1024 1024))
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;;;; cl-lib -- load Common Lisp library:
(require 'cl-lib)

;;; System Local
(defvar my:distrib-id nil)
(defvar my:gentoo-p nil)
(when (file-exists-p "/etc/gentoo-release")
  (setq my:distrib-id "gentoo")
  (setq my:gentoo-p t))

;;; My Functions and Macros -- prefix "my:"
;;;; not eq
(defun my:ne (x y &optional comp)
  (not
   (if comp
       (funcall comp x y)
     (eq x y))))
;;;; like Python's os.path.join()
(defun my:join (a b)
  (concat a
          (when (my:ne (substring a -1) "/" 'string-equal) "/")
          b))
;;;; my:locate-user-emacs-file
(defun my:locate-user-emacs-file (x)
  "Expand filename locate-user-emacs-file"
  (expand-file-name (locate-user-emacs-file x)))
;;;; my:locate-home
(defconst HOME (getenv "HOME"))
(defun my:locate-home (x)
  "Concat and Expand path from HOME"
  (expand-file-name (my:join HOME x)))
;;;; mode enable/disable
(defmacro my:enable-mode (mode)
  `(,mode 1))
(defmacro my:disable-mode (mode)
  `(,mode 0))
;;;; my:add-to-list, add function to hook
(cl-defmacro my:add-to-list (list &optional &body elements)
  `(cl-loop for e in ',elements
            do (add-to-list ',list e)))
(cl-defmacro my:add-function-to-hook (function &optional &body hooks)
  (cl-loop for target-hook in hooks
           do (add-hook (intern (concat (symbol-name target-hook) "-mode-hook"))
                        function)))

;;; My Configurations
;; Name
(setq user-full-name "Hiroyuki Deguchi")
;; E-mail Address
(setq user-mail-address "deguchi@ai.cs.ehime.ac.jp")
;;;; Directory --
;; $HOME/.emacs.d
(when load-file-name                    ; for ``emacs -q -l .emacs''
  (setq user-emacs-directory (file-name-directory load-file-name)))
(defconst my:d:tmp (my:locate-user-emacs-file "tmp"))
(defconst my:d:share (my:locate-user-emacs-file "share"))
;; Nextcloud
(defconst my:d:nextcloud
  (let ((d '("Nextcloud" "nextcloud")))
    (or (cl-find-if
         'file-exists-p
         (mapcar (lambda (x) (my:locate-home x)) d))
        user-emacs-directory)))
;;; Package Management
;; package.el
(require 'package nil t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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
(load custom-file t)

;;; Theme
;;;; Font
;; Cica: https://github.com/miiton/Cica
(when window-system
  (set-fontset-font "fontset-standard" 'unicode (font-spec :family "Cica" :size 16))
  (set-fontset-font nil 'unicode (font-spec :family "Cica" :size 16) nil 'append)
  (set-face-font 'default "fontset-standard")
  (my:add-to-list default-frame-alist (font . "fontset-standard"))
  (setq initial-frame-alist default-frame-alist))
;;;; Color Theme
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))
;;;; Misc.
;; disable menu-bar, tool-bar, scroll-bar
(my:disable-mode menu-bar-mode)
(when window-system
  (my:disable-mode tool-bar-mode)
  (set-scroll-bar-mode 'nil))
;; cursor
(add-to-list 'default-frame-alist '(cursor-type . bar))
;; truncate lines
(setq-default truncate-lines t)
;; transparent-mode
(use-package tp-mode
  :if window-system
  :quelpa (tp-mode :fetcher github :repo de9uch1/tp-mode)
  :config
  (tp-mode 95))
;; display line number
(use-package hlinum
  :ensure t
  :config
  (hlinum-activate))
(if (version<= "26.0.50" emacs-version)
    (progn
      (global-display-line-numbers-mode)
      (setq-default display-line-numbers-width 4))
  (progn
    (my:enable-mode global-linum-mode)
    (setq linum-format "%5d ")))
;; highlight line
(my:enable-mode global-hl-line-mode)
(my:add-function-to-hook
 (lambda () (progn (my:disable-mode linum-mode) (my:disable-mode hl-line-mode)))
 doc-view w3m twittering eshell term)
;; show paren
(my:enable-mode show-paren-mode)
(setq show-paren-style 'mixed)
;;;; mode-line
;; doom-modeline
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-icon t)
  :config
  (my:enable-mode doom-modeline-mode))
;;;; Window Size
(when window-system
  (pcase (system-name)
    ("goedel" (my:add-to-list default-frame-alist (height . 56) (width . 117) )))
  (setq initial-frame-alist default-frame-alist))

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
;;;; misc.
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
;; No startup screen
(setq inhibit-startup-screen t)
;; Bell
;; Alternative flash the screen
(setq visible-bell t)
;; Indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; key binding
(use-package bind-key
  :bind (("C-m" . newline-and-indent)
         ("C-h" . delete-backward-char))) ; C-h -> Backspace
;; .el, .elc
(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))
;; scroll for one line
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; for shell-mode
;; *.~ / .#* no back up
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix (my:join my:d:tmp ".saves-"))
;; definition temporary files and shared files
(setq url-configuration-directory (my:join my:d:tmp "url")
      nsm-settings-file (my:join my:d:tmp "network-settings.data")
      bookmark-default-file (my:join my:d:share "bookmarks"))
;; save minibuffer history
(my:enable-mode savehist-mode)
(setq message-log-max 10000)
(setq history-length t
      savehist-file (my:join my:d:tmp "history"))
;; yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; not add newline at end of buffer
(setq next-line-add-newlines nil)
;; kill whole line when kill line
(setq kill-whole-line t)
;; global-auto-revert-mode
(my:enable-mode global-auto-revert-mode)
;; Emacs Server
(use-package server
  :init
  (defvar server-socket-dir (my:join my:d:tmp "server"))
  :config
  (unless (server-running-p)
    (server-start)))
;; tramp
(use-package tramp
  :config
  (setq tramp-default-method "scp")
  (setq tramp-persistency-file-name (my:join my:d:tmp "tramp")))
;; dired
(use-package dired-aux
  :defer t
  :config
  (use-package dired-async))
(setq dired-dwim-target t) ;; default copy target in 2 windows
(setq dired-recursive-copies 'always) ;; recursive directory copy
(setq dired-isearch-filenames t) ;; only match filenames
;; generic-x
(use-package generic-x)
;; save last opened place
(use-package saveplace
  :config
  (setq save-place-file (my:join my:d:tmp "save-places"))
  (my:enable-mode save-place-mode))
;; for same name buffer
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-min-dir-content 1))
;; XClip
(when window-system
  (setq x-select-enable-clipboard t))
;; popwin
(use-package popwin
  :ensure t
  :config
  (my:enable-mode popwin-mode)
  (my:add-to-list popwin:special-display-config
                  ("*Compile-Log*")
                  ("*Buffer List*")
                  ("*Warnings*")
                  ("*system-packages*")
                  ("*Async Shell Command*")))
(use-package windmove
  :bind (("C-c <left>" . windmove-left)
         ("C-c <right>" . windmove-right)))
;; eldoc
(my:disable-mode global-eldoc-mode)
(use-package eldoc-overlay
  :ensure t
  :config
  (setq eldoc-idle-delay 30))
;; move window
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;;; Global Packages
;;;; eshell
(use-package eshell
  :bind ("M-s" . eshell)
  :hook (eshell-mode . (lambda () (bind-keys :map eshell-mode-map
                                             ("C-d" . delete-char)
                                             ("C-a" . eshell-bol))))
  :config
  (setq my:d:eshell (my:join my:d:tmp "eshell"))
  ;; lastdir & history
  (setq eshell-last-dir-ring-file-name (my:join my:d:eshell (concat "lastdir." (system-name))))
  (setq eshell-history-file-name (my:join my:d:eshell (concat "history." (system-name))))
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
;;;; helm
;; helm-swoop
(use-package helm-swoop
  :ensure t
  :disabled t
  :bind (("C-M-:" . helm-swoop-nomigemo)
         ("M-i" . helm-swoop)
         :map helm-swoop-map
         ("C-r" . helm-previous-line)
         ("C-s" . helm-next-line))
  :config
  (cl-defun helm-swoop-nomigemo (&key $query ($multiline current-prefix-arg))
    "シンボル検索用Migemo無効版helm-swoop"
    (interactive)
    (let ((helm-swoop-pre-input-function
           (lambda () (format "\\_<%s\\_> " (thing-at-point 'symbol)))))
      (helm-swoop :$source (delete '(migemo) (copy-sequence (helm-c-source-swoop)))
                  :$query $query :$multiline $multiline))))

;; prescient
(use-package prescient
  :ensure t
  :custom
  (prescient-aggressive-file-save t)
  (prescient-save-file (my:join my:d:tmp "prescient-save.el"))
  (prescient-history-length 1000)
  :config
  (my:enable-mode prescient-persist-mode))
;; all-the-icons
(use-package all-the-icons
  :ensure t
  :custom
  (all-the-icons-scale-factor 1.0))
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
  (my:enable-mode ivy-mode)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist '((t . "")))
  (setq enable-recursive-minibuffers t)
  ;; all-the-icons-ivy
  (use-package all-the-icons-ivy
    :ensure t)
  (use-package all-the-icons-ivy-rich
    :ensure t
    :config
    (my:enable-mode all-the-icons-ivy-rich-mode))
  ;; ivy-rich
  (use-package ivy-rich
    :ensure t
    :config
    (my:enable-mode ivy-rich-mode))
  ;; ivy-posframe
  (use-package ivy-posframe
    :ensure t
    :custom
    (ivy-posframe-display-functions-alist
     '((t . ivy-posframe-display-at-point)))
    :config
    (my:enable-mode ivy-posframe-mode))
  (use-package ivy-prescient
    :ensure t
    :custom
    (ivy-prescient-retain-classic-highlighting t)
    :config
    (my:enable-mode ivy-prescient-mode)
    ;; (setf (alist-get t ivy-re-builders-alist) #'ivy--regex-ignore-order)
    ))
;; counsel
(use-package counsel
  :ensure t
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("M-y" . counsel-yank-pop)
         ("C-x b" . counsel-ibuffer)
         ("C-M-g" . counsel-ag))
  :custom
  (counsel-yank-pop-separator "\n--------\n")
  :config
  (my:enable-mode counsel-mode))
;; swiper
(use-package swiper
  :ensure t
  :bind (("M-i" . swiper-thing-at-point)
         :map swiper-map
         ("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)))
;;;; avy, ace
(use-package avy
  :ensure t)
;; (use-package avy-migemo
;;   :ensure t
;;   :config
;;   (my:enable-mode avy-migemo-mode)
;;   (use-package avy-migemo-e.g.swiper))
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?h ?j ?k ?l ?u ?i ?o ?p))
  :custom-face
  (aw-leading-char-face ((t (:height 2.0 :forground "#f1fa8c")))))

;; smex
(use-package smex
  :ensure t
  :custom
  (smex-history-length 32)
  (smex-save-file (my:join my:d:tmp "smex-items")))
;;;; company
(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<tab>" . company-complete-selection)
              :map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :init
  (global-company-mode)
  :custom
  (company-idle-delay 0.01)
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 0)
  (company-show-numbers t)
  :config
  (use-package company-flx
    :ensure t
    :config
    (my:enable-mode company-flx-mode)
    )
  (use-package company-prescient
    :ensure t
    :config
    (my:enable-mode company-prescient-mode)
    )
  (use-package company-box
    :ensure t
    :after (company all-the-icons)
    :hook (company-mode . company-box-mode)
    :custom
    (company-box-icons-alist 'company-box-icons-all-the-icons))
  (use-package company-quickhelp
    :ensure t
    :config
    (my:enable-mode company-quickhelp-mode))
  (use-package company-tabnine
    :ensure t
    :config
    (push 'company-tabnine company-backends)
    ;; (company-tabnine-install-binary)
    )
  )
;;;; elscreen
(use-package elscreen
  :ensure t
  :config
  (use-package elscreen-w3m)
  (use-package elscreen-server)
  (use-package elscreen-color-theme)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (setq elscreen-display-screen-number nil)
  (use-package counsel
    :config
    (defun my:elscreen-recentf ()
      (interactive)
      (let ((target-screen (elscreen-get-current-screen)))
        (if (setq target-screen (elscreen-create-internal 'noerror))
            (elscreen-goto target-screen))
        (counsel-recentf)
        ))
    (bind-keys :map elscreen-map ("C-r" . my:elscreen-recentf)))
  (elscreen-start))

;;;; migemo
(use-package migemo
  :ensure t
  :if (and (executable-find "cmigemo")
           (locate-library "migemo"))
  :hook (isearch-mode . migemo-init)
  :init (setq migemo-coding-system 'utf-8-unix)
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (if (file-exists-p "/usr/share/migemo/migemo-dict")
      (setq migemo-dictionary "/usr/share/migemo/migemo-dict"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (migemo-init))

;;;; ddskk
(defvar my:d:skk (my:join my:d:nextcloud "app/SKK"))
(setq skk-large-jisyo (my:join my:d:skk "SKK-JISYO.L+emoji.utf8"))
(use-package skk
  :ensure ddskk
  :bind (([hiragana-katakana] . skk-mode)
         ("C-\\" . skk-mode))
  :init
  (setq skk-user-directory my:d:skk
        skk-jisyo (my:join my:d:skk "jisyo.utf8")
        skk-backup-jisyo (my:join my:d:tmp "SKK/jisyo.bak")
        skk-study-backup-file (my:join my:d:tmp "SKK/study.bak"))
  (setq skk-jisyo-code 'utf-8)
  (setq default-input-method "japanese-skk")
  :config
  (setq skk-extra-jisyo-file-list
        (list
         (my:join my:d:skk "SKK-JISYO.JIS3_4")))
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

;;;; emacs-w3m
(use-package w3m
  :ensure t
  :if (executable-find "w3m")
  :bind (:map w3m-mode-map
              ("S" . w3m-db-history))
  :config
  (defvar my:d:w3m:config (my:join my:d:nextcloud "app/emacs-w3m"))
  (defvar my:d:w3m:tmp (my:join my:d:tmp "w3m"))
  ;; user's files
  (setq w3m-init-file (or load-file-name (buffer-file-name))
        w3m-default-save-directory (my:locate-home "Downloads")
        w3m-bookmark-file (my:join my:d:w3m:config "bookmark")
        w3m-arrived-file (my:join my:d:w3m:tmp "history")
        w3m-cookie-file (my:join my:d:w3m:tmp "cookie")
        w3m-form-textarea-directory (my:join my:d:w3m:tmp "formhist")
        w3m-session-file (my:join my:d:w3m:tmp "sessions")
        w3m-icon-directory (my:join my:d:w3m:tmp "icon")
        w3m-favicon-cache-file (my:join my:d:w3m:tmp "favicon"))
  ;; text encoding
  (setq w3m-default-coding-system 'utf-8
        w3m-bookmark-file-coding-system 'utf-8
        w3m-coding-system-priority-list '(utf-8 shift_jis euc-jp iso-2022-jp cp932))
  ;; inline images
  ;;(setq w3m-default-display-inline-images t)
  (setq w3m-default-display-inline-images nil)
  ;; misc.
  (setq w3m-session-load-last-sessions t)
  (setq w3m-use-tab t)
  (setq w3m-use-favicon t)
  (setq w3m-favicon-use-cache-file t)
  (setq w3m-use-cookies t)
  (setq w3m-keep-arrived-urls 100000)
  (setq w3m-db-history-display-size 100000)
  ;; weather
  (setq w3m-weather-default-area "愛媛県・中予"))

;;;; undo
;; undohist
(use-package undohist
  :ensure t
  :config
  (setq undohist-directory (my:join my:d:tmp "undohist"))
  (undohist-initialize))
;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (my:enable-mode global-undo-tree-mode))

;;;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :defer t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (setq yas-snippet-dirs (list (my:join my:d:share "snippets") yasnippet-snippets-dir))
  (my:enable-mode yas-global-mode)
  ;; Select snippet using helm
  (defun shk-yas/helm-prompt (prompt choices &optional display-fn)
    "Use helm to select a snippet. Put this into `yas-prompt-functions.'"
    (interactive)
    (setq display-fn (or display-fn 'identity))
    (use-package helm-config
      :config
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap)))))))

;;; Programming Language
;;;; projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-mode t)
  (setq projectile-enable-caching t)
)

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
  :hook ((python-mode sh-mode c++-mode) . #'lsp)
  :custom
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)

  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental)
  (lsp-response-timeout 5)

  (lsp-prefer-flymake nil)
  (lsp-enable-snippet t)
  ;; (lsp-enable-indentation nil)
  ;; (lsp-enable-completion-at-point nil)
  :config
  (setq lsp-restart 'auto-restart)
  (use-package company-lsp
    :ensure t
    :custom
    (company-lsp-cache-candidates t)
    (company-lsp-async t)
    ;; (company-lsp-enable-recompletion nil)
    :config
    (push 'company-lsp company-backends)
    )
  (use-package lsp-ui
    :ensure t
    :custom
    (lsp-ui-doc-max-width 60)
    (lsp-ui-doc-max-height 20)
    :config
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))
  )

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

;;;; shell script
(use-package fish-mode
  :ensure t
  :defer t)
(use-package ebuild-mode
  :if my:gentoo-p
  :mode
  (("make.conf" . shell-script-mode)))

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
  (setq reftex-use-external-file-finders t)
  (setq reftex-default-bibliography (list (my:join my:d:nextcloud "lab/tex/nlp.bib"))))

;;;; Lisp -- Common Lisp, Scheme
;;;;; Common Lisp
;; SLIME
(setq quicklisp-directory (my:locate-home "quicklisp"))
(use-package slime
  :if (file-exists-p quicklisp-directory)
  :load-path quicklisp-directory
  :config
  (setq inferior-lisp-program "clisp")
  (load (my:join quicklisp-directory "slime-helper.el")))
;;;;; Scheme
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
(bind-key "C-c S" 'scheme-other-window)

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
  :mode
  (("\\.rb$" . enh-ruby-mode)
   ("\\.rake$" . enh-ruby-mode)
   ("\\.cap$" . enh-ruby-mode)
   ("config.ru$" . enh-ruby-mode)
   ("Rakefile$" . enh-ruby-mode)
   ("Capfile$" . enh-ruby-mode)
   ("Gemfile$" . enh-ruby-mode))
  :config
  (my:add-to-list interpreter-mode-alist ("ruby" . enh-ruby-mode))
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
  :defer t
  :config
  (add-hook 'enh-ruby-mode-hook 'rubocop-mode)
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
;; rcodetools
(use-package rcodetools
  :ensure-system-package (rcodetools . "gem install rcodetools")
  ;; cp rcodetools.el <PATH>
  :bind (:map enh-ruby-mode-map
              ("C-M-i" . rct-complete-symbol)
              ("C-c C-t" . ruby-toggle-buffer)
              ("C-c C-d" . xmp)
              ("C-c C-f" . rct-ri))
  :config
  (setq rct-find-tag-if-available nil)
  (defun make-ruby-scratch-buffer ()
    (with-current-buffer (get-buffer-create "*ruby scratch*")
      (enh-ruby-mode)
      (current-buffer)))
  (defun ruby-scratch ()
    (interactive)
    (pop-to-buffer (make-ruby-scratch-buffer))))

;;;; python
(use-package python-mode
  ;; :ensure-system-package (pip
  ;;                         (python-language-server . "pip install --user python-language-server[all]"))
  :ensure t
  :config
  (setq py-outline-minor-mode-p nil)
  (setq py-current-defun-show nil)
  (setq py-jump-on-exception nil)
  (setq py-current-defun-delay 1000))
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
    (my:add-to-list popwin:special-display-config ("*quickrun*"))))

;;;; outline-(minor-)?mode
(use-package outline
  :init
  (use-package outline-magic
    :ensure t)
  :bind (()
         :map outline-minor-mode-map
         ("<tab>" . outline-cycle)
         ("C-c C-f" . outline-forward-same-level)
         ("C-c C-b" . outline-backward-same-level)
         ("C-c C-n" . outline-next-visible-heading)
         ("C-c C-p" . outline-previous-visible-heading)
         :map outline-mode-map
         ("<tab>" . outline-cycle)))

;;; To Work
;;;; Wanderlust -- E-mail client:
(use-package wl
  :ensure wanderlust
  :disabled t
  :config
  (use-package mime-def)
  (use-package cp5022x
    :ensure t
    :config
    (my:add-to-list mime-charset-coding-system-alist (iso-2022-jp . cp50220)))
  (setq wl-mime-charset 'utf-8)
  (setq mime-situation-examples-file (my:join my:d:tmp "mime-example"))
  (use-package mime-w3m
    :if (executable-find "w3m"))
  (use-package w3m-ems
    :if (executable-find "w3m"))
  (use-package mime-setup
    :if (locate-library "w3m")
    :config
    (setq mime-w3m-safe-url-regexp nil))
  (setq mime-edit-split-message nil)
  (use-package rail
    :if (locate-library "rail")
    :init
    (setq rail-emulate-genjis t))
  (use-package elscreen-wl)
  ;; prefer text/plain than html
  (set-alist 'mime-view-type-subtype-score-alist '(text . html) 0)
  (setq wl-init-file (my:locate-home ".mua/wl-info.el")))

;;;; org-mode
(use-package org
  :mode
  (("\\.org$" . org-mode)
   ("\\.howm$" . org-mode))
  :bind (("C-c l" . org-store-link)
         ("C-c o" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c t" . org-todo))
  :hook (org-mode . turn-on-font-lock)
  :init
  (setq org-directory (my:join my:d:nextcloud "org/"))
  :custom
  (org-latex-pdf-process '("latexmk %f"))
  :config
  (use-package org-install)
  (use-package org-capture)
  (use-package org-protocol)
  (setq org-capture-bookmark nil)
  (setq org-startup-truncated nil)
  (setq org-return-follows-link t)
  ;;(org-remember-insinuate)
  (setq org-default-notes-file (my:join org-directory "agenda.org"))
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
          ("s" "Snippets" entry (file (my:join org-directory "snippets.org"))
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
    (add-to-list 'org-agenda-files (my:join org-directory file)))
  (setq org-export-with-toc nil)
  (setq org-duration-format (quote h:mm))
  ;; org-gcal
  (use-package request
    :ensure t
    :init
    (setq request-storage-directory (my:join my:d:tmp "request"))
    (unless (file-directory-p request-storage-directory)
      (make-directory request-storage-directory)))
  (use-package org-gcal
    :ensure t
    :if (file-directory-p org-directory)
    :commands (org-gcal-fetch org-gcal-sync)
    :init
    (setq org-gcal-dir (my:join my:d:tmp "org-gcal"))
    (unless org-gcal-dir
      (make-directory org-gcal-dir))
    (setq org-gcal-token-file (expand-file-name ".org-gcal-token" org-gcal-dir))
    (setq alert-log-messages t)
    (setq alert-default-style 'log)
    (setq org-gcal-down-days   90) ;; 過去 3 month
    (setq org-gcal-up-days    180) ;; 未来 6 month
    (setq org-gcal-auto-archive nil)
    :config
    (load (my:join my:d:nextcloud "app/org-gcal/token"))
    (setq org-gcal-file-alist `(("de9uch1@gmail.com" . ,(my:join org-directory "schedule.org"))))))

;;; Misc Packages
;;;; twittering-mode
(use-package twittering-mode
  :ensure t
  :config
  (setq twittering-icon-mode t)                ; Show icons
  (setq twittering-timer-interval 300)         ; Update your timeline each 300 seconds (5 minutes)
  (setq twittering-convert-fix-size 35)
  (setq twittering-use-master-password t)
  (setq twittering-private-info-file (my:locate-home ".gnupg/twittering-mode.gpg"))
  (setq twittering-icon-storage-limit t)
  (setq twittering-icon-storage-file (my:join my:d:tmp "twmode-icon"))
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

;;;; gtags -- GNU Global
(use-package helm-gtags
  :ensure t
  :diminish
  :disabled t
  :hook (prog-mode . helm-gtags-mode)
  :bind (:map helm-gtags-mode-map
              ("M-t" . helm-gtags-find-tag)
              ("M-r" . helm-gtags-find-rtag)
              ("M-s" . helm-gtags-find-symbol)
              ("M-p" . helm-gtags-pop-stack))
  :config
  (my:enable-mode helm-gtags-mode))
;;;; Git
;;;; vc-mode
(setq vc-follow-symlinks t)
(setq vc-handled-backends nil)          ; no use vc-mode
;; remove hook
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode)
  (setq git-gutter:handled-backends '(git hg)))
(use-package gitignore-mode
  :ensure t
  :defer t)
(use-package helm-ghq
  :ensure t
  :defer t)
;;;; open-junk-file
(use-package open-junk-file
  :ensure t
  :bind (("C-x j" . open-junk-file))
  :config
  (setq open-junk-file-format (my:locate-home "tmp/junk/%Y-%m-%d-%H%M%S.")))
;;;; recentf
(use-package recentf
  :config
  (setq recentf-save-file (my:join my:d:tmp "recentf"))
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 5000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 100)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 3000 t 'recentf-save-list))
  (my:enable-mode recentf-mode)
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
  :config
  (setq anzu-use-migemo t)
  (setq anzu-search-threshold 3000)
  (my:enable-mode global-anzu-mode))
;;;; highlight-indentation
(use-package highlight-indentation
  :ensure t
  :hook (html-mode . highlight-indentation-mode)
  :config
  ;; (setq highlight-indentation-offset 4)
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#e3e3d3"))
;;;; expand-region
(use-package expand-region
  :ensure t
  :bind (("C-SPC" . er/expand-region)
         ("C-M-SPC" . er/contract-region)
         ("C-@" . er/expand-region)
         ("C-M-@" . er/contract-region))
  :config
  (setq shift-select-mode nil)
  (setq transient-mark-mode t)          ; transient-mark-mode が nil では動作しない
  (setq expand-region-smart-cursor t))
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
  (setq mc/list-file (my:join my:d:tmp "mc-lists.el")))
;;;; smartparens
(use-package smartparens
  :ensure t
  :diminish
  :config
  (use-package smartparens-config)
  (my:enable-mode smartparens-global-mode))
;;;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))
;;;; multi-term
(use-package multi-term
  :ensure t
  :defer t
  :config
  (setq multi-term-program (executable-find "bash")))

;; Local Variables:
;; coding: utf-8-unix
;; mode: emacs-lisp
;; mode: outline-minor
;; time-stamp-pattern: "10/Modified:\\\\?[ \t]+%:y-%02m-%02d\\\\?\n"
;; End:

;;; init.el ends here
