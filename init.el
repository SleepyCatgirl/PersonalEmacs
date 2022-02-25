;; Simplify UI
(setq inhibit-startup-message t)
(scroll-bar-mode -1)    ; Disable visual scrollbar
(tool-bar-mode -1)      ; Disable toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Give some breathing room

(menu-bar-mode -1)      ; disable menu bar
;; Prettier title format
(setq frame-title-format '("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " - Emacs"))

;; Set up visible bell
(setq visible-bell t)

;; Setting font
(set-face-attribute 'default nil :font "SF Mono Powerline" :height 120)

;; Set pretty theme
;;(load-theme 'wombat)
;; Make ESC quit prompts (global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Remembmer last place visited in file
(save-place-mode 1)

;; Move customization vars to separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Dont pop up UI dialogs
(setq use-dialog-box nil)
;; Use minibuffer for pgp passphase instead of GTK window
(setq epg-pinentry-mode 'loopback)
;; Revert buffer if file on disk has been changed
;; 0 for no, 1 for yes
(global-auto-revert-mode 1)


;; Remember recently edited files (recentf)
(recentf-mode 1)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;; Initialize use package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
;; use package
(require 'use-package)


(setq use-package-always-ensure t)
;; Install diminish utility for use-package
;; That hides from list of enabled modes
(use-package diminish)

;; undo tree
(use-package undo-tree)
(global-undo-tree-mode)
;; Ivy completion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map ;; :map - mode specific binds
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;;(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
;;(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)
;; Ivy icons
(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))
;; extra information in ivy
(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-rich-path-style 'abbrev))


;; Sorting in ivy and such
(use-package prescient
  :config
  (prescient-persist-mode 1))

;; Counsel, better versions of emacs utilities
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :config
  (counsel-mode 1))

;; ivy prescient, after counsel
(use-package ivy-prescient
  :config
  (ivy-prescient-mode 1))

;; Doom themes
(use-package doom-themes
  :config
  (doom-themes-org-config)
  (load-theme 'doom-gruvbox t))
;; doom mode line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 32)
  (doom-modeline-width 4))

;; Vim bindings

(defun rune/evil-hook ()
  (dolist (mode '(eshell-mode
                  erc-mode))
    (evil-set-initial-state mode 'normal)))
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-i-jump nil)
  (evil-mode 1)
  :hook (evil-mode . rune/evil-hook)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; use visual line motions even outside of visual-line-mode bufers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; line numbers for better moving around code
(column-number-mode) ;; keep track of column
(global-display-line-numbers-mode t)
;; Disable them for some
(dolist (mode '(term-mode-hook
                eshell-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Rainbow delimeters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :init (setq rainbow-delimiters-mode 1))
;(add-hook 'prog-mode-hook #'rainbow-delimeters-mode)

;; Which key, panel popup that shows what keys are available
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Default IRC libera chat
(setq erc-default-server "irc.libera.chat")
;; IRC Image
(use-package erc-image)
(use-package erc-colorize)
(require 'erc-image)
(add-to-list 'erc-modules 'image)
(erc-update-modules)


;; augmentation to help system
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-funcion] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; org
(use-package org
  :config
  (setq org-hide-leading-stars 1)
  (setq org-indent-mode 1)
  (setq org-startup-indented t)
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files '("~/org-roam/20220102222924-to_do.org"))
  :hook (org-mode . visual-line-mode))
;(setq org-todo-keywords '((sequence "(TODO(t)" "|" "DONE(d)")
;			  (sequence "(KILL(k)")))
(require 'org-tempo)
;; Org babel
(org-babel-do-load-languages 'org-babel-load-languages
			     '((shell .t)
			       (C . t)
			       (lisp . t)))
(setq org-babel-lisp-eval-fn 'sly-eval)
;; Dont ask for eval conf
(setq org-confirm-babel-evaluate nil)
;; org roam
(use-package org-roam)
;;(make-directory "~/org-roam")
(setq org-roam-directory (file-truename "~/org-roam"))
(org-roam-db-autosync-mode)
;; LaTeX Preview size
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
;; Latex imagemagick + tikz
(setq org-latex-create-formula-image-program 'imagemagick)
(setq org-latex-packages-alist
      '(("" "tikz" t)
        ("" "tikz-cd" t)))
;; Pomodoro
;;(setq org-clock-sound "")

;; solaier mode for aesthics
(use-package solaire-mode
  :demand
  :init
  :config
  (solaire-global-mode +1))


;; keybinding package
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(general-define-key
 "C-M-j" 'counsel-switch-buffer
 "C-s" 'counsel-grep-or-swiper)


;; hydra - transient bindings
(use-package hydra)

;; example, text scaling
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Nix
(use-package nix-mode
  :mode "\\.nix\\'")


;; Emacs project interation

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Personal")
    (setq projectile-project-search-path '("~/Projects/Personal")))
  (setq projectile-switch-project-action #'projectile-dired))
;; projectile counsel integration
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Programming
;; Git
(use-package transient)
(use-package magit
  :after transient
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Programming modes
; Haskell
(use-package haskell-mode
  :hook (haskell-mode . interactive-haskell-mode))
;; Use stack for haskell repl
(setq haskell-process-type 'stack-ghci)
;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; Common Lisp 
(use-package sly)
(setq inferior-lisp-program "/run/current-system/sw/bin/sbcl")
(use-package paredit
  :hook (lisp-mode . paredit-mode))
(use-package smartparens
  :hook (prog-mode . smartparens-mode))
;; LSP
(use-package company) ;; lsp autocompletion
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :config (lsp-enable-which-key-integration t))
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)
;; C++
(use-package ccls
  :hook (c++-mode . lsp-deferred)
        (c++-mode . company-mode))
;; Haskell
(use-package lsp-haskell
  :hook (haskell-mode . lsp-deferred)
  (haskell-literate-mode . lsp-deferred)
  (haskell-mode . company-mode)
  (haskell-literate-mode . company-mode))
;; Clojure
(use-package clojure-mode)
;  :hook (clojure-mode . lsp-deferred)
;       (clojurescript-mode-hook . lsp-deferred))
(use-package cider)

;; PDF
(use-package pdf-tools
  :init (pdf-tools-install))


;; Vterm - better terminal
;; Replace in CmakeLists.txt:
;;        libvterm.a -> libvterm.so
;;        STATIC -> SHARED
					;(use-package vterm)

;; Web dev
(use-package skewer-mode
  :hook (js2-mode . skewer-mode)
  (html-mode . skewer-html-mode)
  (css-mode . skewer-css-mode))
(use-package js2-mode)
(use-package css-mode)  
;; Live html
;; Start server with httpd-start
;; use impatient-mode on buffers
(use-package impatient-mode)
;;(setq httpd-root "/home/senchou/Programming/HTML/Nya/")



;; Music, emms -> MPD
;;(use-package emms
;;  :ensure t
;;  :config
;;  (emms-all)
;;  (require 'emms-player-mpd)
;;  (require 'emms-setup)
;;;;  (setq emms-source-file-default-directory "~/Music")
;;;; (emms-add-directory-tree "~/Music")
;;  ;;  (add-to-list 'emms-info-functions 'emms-info-mpd)
;;  (add-to-list 'emms-info-functions 'emms-info-mpd)
;;  (add-to-list 'emms-player-list 'emms-player-mpd)
;;  (setq emms-player-server-port "6600")
;;  (setq emms-player-mpd-music-directory "~/Music")
;;  (emms-player-mpd-connect))
;;;  :bind (:map Emms-Browser
;	      ("C-x C-f" . emms-browser-add-tracks-and-play)))
;  ("C-x C-p" . emms-player-mpd-pause)
;  ("C-x C-n" . emms-player-mpd-next)
;  ("C-x C-e" . emms-browser-add-tracks-and-play))  


