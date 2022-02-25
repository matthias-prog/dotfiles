(scroll-bar-mode -1) ; (set-fringe-mode 20)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(set-default-coding-systems 'utf-8)

(set-face-attribute 'default nil :font "JetBrains Mono")

;; ESC now escapes prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; remap the normal switch buffer command to the counsel version
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)

(column-number-mode) ; adds columnnumbers to the mode line
(global-display-line-numbers-mode t) ; add linenumbers

;; disable linenumbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-hl-line-mode 1) ; highlight current line

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Set up indentation
(setq-default indent-tabs-mode nil) ; all spaces
(setq-default tab-width 2)

;; Inititalize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; menus for everything important
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
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
  :init
  (ivy-mode 1))

;; improves some standard functions
(use-package counsel
  :init (counsel-mode 1) ; auto remaps all standard functions to counsel functions
  :config
  (setq ivy-initial-inputs-alist nil)) ; removes the dumb ^ from the prompts 

;; keybindings and doc-strings in ivy-menus
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; better search - plug in to ivy
(use-package swiper)

;; better documentation
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 30))

;; dependency for doom-modeline
(use-package all-the-icons
  :if (display-graphic-p)) ; don't forget to run M-x all-the-icons-install-fonts when installing!!!

;; give dired some nice icons from the "all the icons" package
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; adds colored parentheses 
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; emoji support for emacs
(use-package emojify
  :hook (after-init . global-emojify-mode))

;; useful prompt showing all key combos
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.5))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)
  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; crazy keymap package
(use-package general
  :config
  (general-create-definer rune/leader-keys ;; create own leader key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC" ;; works from normal mode and stuff
    :global-prefix "C-SPC") ;; works everywhere
  (rune/leader-keys  ;; mostly an example
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose-theme")))


;; transient keymaps
(use-package hydra)

;; creates an function which creates the transient keymap
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; adds the hydra function to the keymap from the general package
(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; our own "hook" that enables emacs-mode/disables evil for the listed modes
(defun rune/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  erc-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :hook (evil-mode . rune/evil-hook)
  :init
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

;; good evil keybindings for many other (third-party) modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Common Lisp setup
(use-package slime)
(setq inferior-lisp-program "/usr/bin/sbcl")

;; Lua setup
(use-package lua-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(ivy-mode t)
 '(package-selected-packages
   '(hydra general evil-collection evil all-the-icons-dired emojify lua-mode slime helpful counsel ivy-rich which-key rainbow-delimiters doom-themes doom-modeline ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
