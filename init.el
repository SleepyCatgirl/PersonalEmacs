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
  (setq visual-line-mode 1)
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t))
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


;; solaier mode for aesthics
(use-package solaire-mode
  :init (solaire-global-mode +1))


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
;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; Common Lisp 
(use-package sly)
(setq inferior-lisp-program "/run/current-system/sw/bin/sbcl")
(use-package paredit
  :hook (lisp-mode . paredit-mode))
(use-package smartparens
  :hook (prog-mode . smartparens-mode))


;; PDF
(use-package pdf-tools
  :init (pdf-tools-install))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(forge which-key use-package solaire-mode smartparens sly rainbow-delimiters pdf-tools paredit org-roam nix-mode magit macrostep ivy-rich ivy-prescient hydra helpful haskell-mode general evil-collection erc-image erc-colorize doom-themes doom-modeline diminish counsel-projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
