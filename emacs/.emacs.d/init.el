(setq user-emacs-directory (file-truename "~/dotfiles/emacs/.emacs.d/"))
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                 (not (gnutls-available-p))))
    (proto (if no-ssl "http" "https")))
    ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
(add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

;;; -*- lexical-binding: t -*-

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(eval-when-compile
  (setq use-package-expand-minimally byte-compile-current-file))

;;(toggle-frame-fullscreen)

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

  ;(load-theme 'tsdh-light)
(use-package dracula-theme
  :ensure t
  :demand
  :config (load-theme 'dracula t))

(set-face-attribute 'default nil :font "Fira Code Retina 12")
(setq-default line-spacing 0)
(setq initial-frame-alist '((top . 50) (left . 100) (width . 210) (height . 70)))
(tool-bar-mode -1)

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (setq sp-show-pair-delay 0)

  ;; no '' pair in emacs-lisp-mode
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'markdown-mode "`"   nil :actions '(wrap insert))  ;; only use ` for wrap and auto insertion in markdown-mode
  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp) ;; something strange
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp))  ;; since it's mac default keybinding

  (global-visual-line-mode 1)

(setq column-number-mode t) ;; show columns in addition to rows in mode line

(global-display-line-numbers-mode t)

(setq-default frame-title-format "%b (%f)")

(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq-default c-basic-offset 2)
(setq c-basic-offset 2)
(setq-default tab-width 2)
(setq-default c-basic-indent 2)

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(blink-cursor-mode 0)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; stop creating .# files

(global-auto-revert-mode t)

(setq
 inhibit-startup-message t         ; Don't show the startup message
 inhibit-startup-screen t          ; or screen
 cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows

 echo-keystrokes 0.1               ; Show keystrokes right away, don't show the message in the scratch buffe
 initial-scratch-message nil       ; Empty scratch buffer
 initial-major-mode 'org-mode      ; org mode by default
 sentence-end-double-space nil     ; Sentences should end in one space, come on!
 confirm-kill-emacs 'y-or-n-p      ; y and n instead of yes and no when quitting
 ;; help-window-select t              ; select help window so it's easy to quit it with 'q'
)

(fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-unset-key (kbd "s-p"))
(global-hl-line-mode nil)

(use-package simpleclip
  :config
  (simpleclip-mode 1))

(setq ring-bell-function 'ignore)

(setq scroll-margin 10
   scroll-step 1
   next-line-add-newlines nil
   scroll-conservatively 10000
   scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(use-package super-save
  :config
  (super-save-mode +1))

(use-package exec-path-from-shell)


(use-package plantuml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(use-package shell-pop)

(global-set-key (kbd "s-<backspace>") 'kill-whole-line)
(global-set-key (kbd "M-S-<backspace>") 'kill-word)

(global-set-key (kbd "s-<right>") (kbd "C-e"))
(global-set-key (kbd "S-s-<right>") (kbd "C-S-e"))
(global-set-key (kbd "s-<left>") (kbd "M-m"))
(global-set-key (kbd "S-s-<left>") (kbd "M-S-m"))

(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

(global-set-key (kbd "s-a") 'mark-whole-buffer)       ;; select all
(global-set-key (kbd "s-s") 'save-buffer)             ;; save
(global-set-key (kbd "s-S") 'write-file)              ;; save as
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ;; quit

(global-set-key (kbd "s-z") 'undo)

(defun my-pop-local-mark-ring ()
  (interactive)
  (set-mark-command t))

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))

(global-set-key (kbd "s-,") 'my-pop-local-mark-ring)
(global-set-key (kbd "s-.") 'unpop-to-mark-command)

(global-set-key (kbd "s-<") 'previous-buffer)
(global-set-key (kbd "s->") 'next-buffer)

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "s-o") (kbd "C-x o"))

(global-set-key (kbd "s-w") (kbd "C-x 0")) ;; just like close tab in a web browser
(global-set-key (kbd "s-W") (kbd "C-x 1")) ;; close others with shift

(global-set-key (kbd "s-T") 'vsplit-last-buffer)
(global-set-key (kbd "s-t") 'hsplit-last-buffer)

(use-package expand-region
  :config
  (global-set-key (kbd "s-'") 'er/expand-region))

(use-package move-text
  :config
  (move-text-default-bindings))

(defun smart-open-line ()
  "Insert an empty line after the current line. Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line. Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "s-<return>") 'smart-open-line)
(global-set-key (kbd "s-S-<return>") 'smart-open-line-above)

(defun smart-join-line (beg end)
  "If in a region, join all the lines in it. If not, join the current line with the next line."
  (interactive "r")
  (if mark-active
      (join-region beg end)
      (top-join-line)))

(defun top-join-line ()
  "Join the current line with the next line."
  (interactive)
  (delete-indentation 1))

(defun join-region (beg end)
  "Join all the lines in the region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))

(global-set-key (kbd "s-j") 'smart-join-line)
;; (global-set-key (kbd "s-J") 'smart-join-line)

;; (global-set-key (kbd "s-i") 'previous-line)
;; (global-set-key (kbd "s-k") 'next-line)
;; (global-set-key (kbd "s-j") 'left-char)
;; (global-set-key (kbd "s-l") 'right-char)

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

(use-package visual-regexp
  :config
  (define-key global-map (kbd "s-r") 'vr/replace))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all 1)
  (global-set-key (kbd "s-d") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-s-d") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "s-D") 'mc/mark-all-dwim)
  (define-key mc/keymap (kbd "<return>") nil))

(global-set-key (kbd "s-/") 'comment-line)

(put 'dired-find-alternate-file 'disabled nil)

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always))

(setq split-height-threshold 0)
(setq split-width-threshold nil)

(use-package windmove
  :config
  (global-set-key (kbd "s-[")  'windmove-left)         ;; Cmd+[ go to left window
  (global-set-key (kbd "s-]")  'windmove-right)        ;; Cmd+] go to right window
  (global-set-key (kbd "s-{")  'windmove-up)           ;; Cmd+Shift+[ go to upper window
  (global-set-key (kbd "s-}")  'windmove-down))      ;; Ctrl+Shift+[ go to down window

(winner-mode 1)
(global-set-key (kbd "C-s-[") 'winner-undo)
(global-set-key (kbd "C-s-]") 'winner-redo)

(use-package shackle
  :init
  (setq shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-rules '((help-mode           :align below :select t)
                        (helpful-mode        :align below)
                        (dired-mode          :ignore t)

                        (compilation-mode    :select t   :size 0.25)
                        ("*compilation*"     :select nil :size 0.25)
                        ("*ag searcph*"       :select nil :size 0.25)
                        ("*Flycheck errors*" :select nil :size 0.25)
                        ("*Warnings*"        :select nil :size 0.25)
                        ("*Error*"           :select nil :size 0.25)

                        ("*Org Links*"       :select nil   :size 0.2)

                        (neotree-mode                     :align left)
                        (magit-status-mode                :align bottom :size 0.5  :inhibit-window-quit t)
                        (magit-log-mode                   :same t                  :inhibit-window-quit t)
                        (magit-commit-mode                :ignore t)
                        (magit-diff-mode     :select nil  :align left   :size 0.5)
                        (git-commit-mode                  :same t)
                        (vc-annotate-mode                 :same t)
                        ("^\\*git-gutter.+\\*$" :regexp t :size 15 :noselect t)
                        ))
  :config
  (shackle-mode 1))
  ;; (defun my/shackle-defaults (plist)
  ;;   "Ensure popups are always aligned and selected by default. Eliminates the need
  ;;  for :align t on every rule."
  ;;   (when plist
  ;;     (unless (or (plist-member plist :align)
  ;;                 (plist-member plist :same)
  ;;                 (plist-member plist :frame))
  ;;       (plist-put plist :align t))
  ;;     (unless (or (plist-member plist :select)
  ;;                 (plist-member plist :noselect))
  ;;       (plist-put plist :select t)))
  ;;   plist)
  ;; (advice-add #'shackle--match :filter-return #'my/shackle-defaults)

  ;; (add-hook 'my/after-init-hook 'shackle-mode))

(global-unset-key (kbd "s-n"))

;; Go to other windows easily with one keystroke Cmd-something.
(global-set-key (kbd "s-1") (kbd "C-x 1"))  ;; Cmd-1 kill other windows (keep 1)
(global-set-key (kbd "s-2") (kbd "C-x 2"))  ;; Cmd-2 split horizontally
(global-set-key (kbd "s-3") (kbd "C-x 3"))  ;; Cmd-3 split vertically
(global-set-key (kbd "s-0") (kbd "C-x 0"))  ;; Cmd-0...
(global-set-key (kbd "s-w") (kbd "C-x 0"))  ;; ...and Cmd-w to close current window

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-P") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-project-search-path '("/Users/sqrel/Code/" "/Users/sqrel/Code/matic")))

(use-package helm-swoop
  :config
  (global-set-key (kbd "s-f") 'helm-swoop))

(use-package helm
  :config
  (require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (setq helm-follow-mode-persistent t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-split-window-inside-p t)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "s-b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  )
(setq helm-swoop-pre-input-function
      (lambda () ""))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package helm-ag
  :config
  (global-set-key (kbd "s-F") 'helm-projectile-ag))

(global-set-key (kbd "s-p") 'helm-projectile-find-file)

(use-package magit
  :config
  (global-set-key (kbd "s-g") 'magit-status))

(use-package git-gutter
  :diminish
  :config
  (global-git-gutter-mode 't)
  (set-face-background 'git-gutter:modified 'nil)   ;; background color
  (set-face-foreground 'git-gutter:added "green4")
  (set-face-foreground 'git-gutter:deleted "red"))

(use-package neotree
  :config
  (setq neo-window-width 32
        neo-create-file-auto-open t
        neo-banner-message nil
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-show-hidden-files t
        neo-mode-line-type 'none
        neo-auto-indent-point t)
  (setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
  (global-set-key (kbd "s-B") 'neotree-toggle))

(setq ispell-program-name "aspell")

(add-hook 'text-mode-hook 'flyspell-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

(global-set-key (kbd "s-\\") 'ispell-word)

(use-package powerthesaurus
  :config
  (global-set-key (kbd "s-|") 'powerthesaurus-lookup-word-dwim))

(use-package define-word
  :config
  (global-set-key (kbd "M-\\") 'define-word-at-point))

(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode markdown-mode))
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package yaml-mode)
(use-package markdown-mode)
(use-package haml-mode)
(use-package cider)
(use-package dumb-jump
  :config
  (dumb-jump-mode))
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")
(use-package rspec-mode
  :config
  (require 'rspec-mode))
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (package-install 'exec-path-from-shell)
  (exec-path-from-shell-initialize))
(use-package clojure-mode)
(use-package dockerfile-mode
  :config
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (add-to-list 'auto-mode-alist '("\\.docker\\'" . dockerfile-mode)))
(use-package yafolding :defer t
  :hook ((prog-mode . yafolding-mode)))

(use-package web-mode
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2))

(use-package emmet-mode
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

  (use-package htmlize)

(defun fterm ()
  "Open fish terminal"
  (interactive)
  (ansi-term "/usr/local/bin/fish"))

(use-package org
  :config
  (setq org-startup-indented t))

(setq org-directory "~/Dropbox/me.txt/org")

(setq org-agenda-files '("~/Dropbox/me.txt/org"))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-support-shift-select t)

(eval-after-load 'org
  '(progn
    (add-to-list 'org-structure-template-alist '("el" "#+BEGIN_SRC emacs-lisp \n?\n#+END_SRC"))
    (define-key org-mode-map (kbd "C-'") nil)
    (global-set-key "\C-ca" 'org-agenda)))

(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)

;(find-file "~/Dropbox/me.txt/org/main.org")

(setq org-log-into-drawer t)

(defun org-mode-export-links ()
  "Export links document to HTML automatically when 'links.org' is changed"
  (when (equal (buffer-file-name) "/Users/sqrel/Dropbox/me.txt/org/links.org")
    (progn
      (org-html-export-to-html)
      (message "HTML exported"))))

(add-hook 'after-save-hook 'org-mode-export-links)

(global-set-key (kbd "\e\ec") (lambda () (interactive) (find-file "~/.emacs.d/init.org")))
(global-set-key (kbd "\e\er") (lambda () (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key (kbd "\e\em") (lambda () (interactive) (find-file "~/Dropbox/me.txt/org/main.org")))
(global-set-key (kbd "\e\el") (lambda () (interactive) (find-file "~/Dropbox/me.txt/org/links.org")))
(global-set-key (kbd "\e\ew") (lambda () (interactive) (find-file "~/Dropbox/me.txt/org/work.org")))
(global-set-key (kbd "\e\et") (lambda () (interactive) (find-file "~/Code/test.rb")))

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-cycle-separator-lines 1)

(setq org-log-done 'time)

;; no shift or alt with arrows
(define-key org-mode-map (kbd "<S-left>") nil)
(define-key org-mode-map (kbd "<S-right>") nil)
(define-key org-mode-map (kbd "<M-left>") nil)
(define-key org-mode-map (kbd "<M-right>") nil)
;; no shift-alt with arrows
(define-key org-mode-map (kbd "<M-S-left>") nil)
(define-key org-mode-map (kbd "<M-S-right>") nil)

(define-key org-mode-map (kbd "C-c C-,") 'org-metaleft)
(define-key org-mode-map (kbd "C-c C-.") 'org-metaright)

(setq org-use-speed-commands t)

(setq org-capture-templates
      (quote (
               ;; (("t"
               ;;   "TODO"
               ;;   entry
               ;;   (file+olp "inbox.org" "Tasks")
               ;;   "* TODO %?\n%U\n%a\n")

               ("n"
                 "Note"
                 entry
                 (file+olp "main.org" "Notes Inbox")
                 "* %?\n%U\n%a\n")
               ("j"
                 "Journal"
                 entry
                 (file+datetree "journal.org")
                 "* %U\n%?")
               ("w"
                "Work Todo"
                entry
                (file+datetree "work.org")
                "* TODO %?\n----Entered on %U"))
               ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode yafolding which-key web-mode visual-regexp use-package super-save smartparens simpleclip shell-pop shackle rspec-mode powerthesaurus plantuml-mode neotree multiple-cursors move-text markdown-mode magit htmlize helm-swoop helm-projectile helm-ag haml-mode git-gutter flycheck expand-region exec-path-from-shell emmet-mode dumb-jump dracula-theme dockerfile-mode define-word company cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
