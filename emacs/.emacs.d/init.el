;; Default emacs path since i'm using Stow to populate configs
(setq user-emacs-directory (file-truename "~/dotfiles/emacs/.emacs.d/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initialize use-package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;; Now you don't need to write ensure t since it is default
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Automatically compile init.el file on save

(defun byte-compile-init-files (file)
  "Automatically compile FILE."
  (interactive)
  (save-restriction
    ;; Suppress the warning when you setq an undefined variable.
    (setq byte-compile-warnings '(not free-vars obsolete))
    (byte-compile-file (expand-file-name file))))

(add-hook
 'after-save-hook
 (function
  (lambda ()
    (if (string= (concat user-emacs-directory "init.el")
                 (file-truename (buffer-file-name)))
        (byte-compile-init-files (concat user-emacs-directory "init.el"))))))

;; END


;; Open Emacs in fullscreen always
(if (not (eq (frame-parameter nil 'fullscreen) 'fullboth))
    (toggle-frame-fullscreen) nil)


;; Use macos dark theme
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; One of the best colorschemas
(use-package gruvbox-theme
  :ensure t
  :demand
  :config (load-theme 'gruvbox t))

;; Setup font settings
(set-face-attribute 'default nil :font "Fira Code Retina 13")
(setq-default line-spacing 0)

;; Disable toolbar
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

(global-display-line-numbers-mode 0)

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

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; stop creating .# files

(global-auto-revert-mode t)

(setq
 inhibit-startup-message t         ; Don't show the startup message
 inhibit-startup-screen t          ; or screen
 cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows

 echo-keystrokes 0.1               ; Show keystrokes right away, don't show the message in the scratch buffe
 ;initial-scratch-message nil       ; Empty scratch buffer
 sentence-end-double-space nil     ; Sentences should end in one space, come on!
 confirm-kill-emacs 'y-or-n-p      ; y and n instead of yes and no when quitting
 ;; help-window-select t              ; select help window so it's easy to quit it with 'q'
)

(fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-unset-key (kbd "s-n"))
(global-unset-key (kbd "s-p"))
(global-hl-line-mode nil)

(use-package simpleclip
  :config
  (simpleclip-mode 1))

(setq ring-bell-function 'ignore)

(use-package super-save
  :config
  (super-save-mode +1))

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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

(global-set-key (kbd "s-,") 'previous-buffer)
(global-set-key (kbd "s-.") 'next-buffer)

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

(use-package visual-regexp
  :config
  (define-key global-map (kbd "s-r") 'vr/replace))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(global-set-key (kbd "s-/") 'comment-line)

(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(use-package windmove
  :config
  (global-set-key (kbd "s-[")  'windmove-left)         ;; Cmd+[ go to left window
  (global-set-key (kbd "s-]")  'windmove-right)        ;; Cmd+] go to right window
  (global-set-key (kbd "s-{")  'windmove-up)           ;; Cmd+Shift+[ go to upper window
  (global-set-key (kbd "s-}")  'windmove-down))      ;; Ctrl+Shift+[ go to down window

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
  (setq projectile-project-search-path '("/Users/vasylmelnychuk/projects/" "/Users/vasylmelnychuk/projects/toptal" "/Users/vasylmelnychuk/projects/vendors/"))
  (setq projectile-enable-caching t))
  ;;(setq projectile-indexing-method 'native))

;; Use minimalist Ivy for most things.
(use-package ivy
  :diminish                             ;; don't show Ivy in minor mode list
  :config
  (ivy-mode 1)                          ;; enable Ivy everywhere
  (setq ivy-use-virtual-buffers t)      ;; show bookmarks and recent files in buffer list
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))
  (global-set-key (kbd "s-b") 'ivy-switch-buffer))  ;; Cmd+b show buffers and recent files


;; Swiper is a better local finder.
(use-package swiper
  :config
  (global-set-key (kbd "s-f") 'swiper)) ;; Cmd+f find text


;; Better menus with Counsel (a layer on top of Ivy)
(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)            ;; Alt+x run command
  (global-set-key (kbd "s-P") 'counsel-M-x)            ;; Cmd+Shift+p run command
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)  ;; Replace built-in Emacs 'find file' (open file) with Counsel
  (global-set-key (kbd "s-o") 'counsel-find-file))     ;; Cmd+o open file

(use-package smex)  ;; show recent commands when invoking Alt-x (or Cmd+Shift+p)
(use-package flx)
;; Make Ivy a bit more friendly by adding information to ivy buffers, e.g. description of commands in Alt-x, meta info when switching buffers, etc.
(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev)) ;; Abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)


;; Integrate Projectile with Counsel
(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "s-p") 'counsel-projectile-find-file)         ;; Cmd+p open file in current project
  (global-set-key (kbd "s-F") 'counsel-projectile-ag))     ;; Cmd+Shift+F search in current git repository

(setq projectile-completion-system 'ivy)             ;; Use Ivy in Projectile

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
        ;neo-vc-integration 'char
        neo-auto-indent-point t)
  (setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
  (global-set-key (kbd "s-B") 'neotree-toggle)
  (add-hook 'neo-after-create-hook
  #'(lambda (_)
       (with-current-buffer (get-buffer neo-buffer-name)
         (setq truncate-lines t)
         (setq word-wrap nil)
         (make-local-variable 'auto-hscroll-mode)
         (setq auto-hscroll-mode nil)))))

(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode markdown-mode))
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package yaml-mode)
(use-package markdown-mode
  :config
  (custom-set-variables
   '(markdown-command "/usr/local/bin/pandoc")))
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
(use-package feature-mode
  :config
  (require 'feature-mode))
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

(use-package elixir-mode
  :config
  (require 'elixir-mode))
(use-package alchemist)

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun fterm ()
  "Open fish terminal"
  (interactive)
  (ansi-term "/usr/local/bin/fish"))

(global-set-key (kbd "\e\ec") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "\e\ek") (lambda () (interactive) (find-file "~/projects/knowledge/index.org")))
(global-set-key (kbd "\e\er") (lambda () (interactive) (load-file "~/.emacs.d/init.el")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (edit-indirect alchemist elixir-mode yaml-mode yafolding which-key web-mode visual-regexp use-package super-save smex smartparens simpleclip rspec-mode neotree markdown-mode magit ivy-rich haml-mode gruvbox-theme git-gutter flx feature-mode exec-path-from-shell dumb-jump dockerfile-mode counsel-projectile company cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq mac-system-move-file-to-trash-use-finder nil)
