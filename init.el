;;; -*- lexical-binding: t; -*-

;;; TODO:
;; port spc q
;; configure: cl, rust, c, clojure, latex, lua, markdown, js, nix, python, sh
;; snippets
;; magit
;; org, org-roam
;; debugger (dap-mode?)
;; lsp
;; rss


(eval-and-compile
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-hook-name-suffix nil))


;;; Init packages
(require 'package)
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up

;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents)               ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

;; REVIEW
;; (setq use-package-always-ensure t)

;; Setup use-package
(eval-when-compile
  (require 'use-package))


;;; quelpa
(use-package quelpa
  :ensure t
  :defer t
  :custom
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

;;; use-package-quelpa
(use-package quelpa-use-package
  :ensure t
  :init (setq quelpa-use-package-inhibit-loading-quelpa t))

;;; use-package custom-update
;; To be able update lists in custom
(use-package use-package-custom-update
  :quelpa
  (use-package-custom-update
   :repo "a13/use-package-custom-update"
   :fetcher github
   :version original))

;;; Garbage Collector Magic Hack
(use-package gcmh
  :ensure t
  :init
  (gcmh-mode 1))

;;; This one tries to speed up Emacs startup a little bit
(use-package fnhh
  :quelpa
  (fnhh :repo "a13/fnhh" :fetcher github)
  :config
  (fnhh-mode 1))

;;; Modernized Package Menu
;; REVIEW
;; (use-package paradox
;;   :ensure t
;;   :defer 1
;;   :config
;;   (paradox-enable))

;;;; Utils
(defun kei/notify-send (title message)
  "Display a desktop notification by shelling MESSAGE with TITLE out to `notify-send'."
  (call-process-shell-command (format "notify-send -t 2000 \"%s\" \"%s\"" title message)))


;;; Interface tweaks
(use-package emacs
  :defer t
  :custom
  (indent-tabs-mode nil)
  (tab-width 4)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (scroll-step 1)
  (auto-window-vscroll nil)
  (tooltip-mode nil)
  (enable-recursive-minibuffers t)
  (completion-ignore-case t)
  (sentence-end-double-space nil)
  (column-number-mode t)
  (read-buffer-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (resize-mini-frames nil)
  (resize-mini-windows nil)
  (default-frame-alist '((menu-bar-lines . 0)
                         (tool-bar-lines . 0)
                         (scroll-bar . nil)
                         (vertical-scroll-bars . nil)
                         (left-fringe . 5)
                         (right-fringe . 0)))
  (user-full-name "Druk Oleksandr")
  :config
  (defun kei/toggle-line-numbers ()
    "Toggle line numbers.

Cycles through regular, relative and no line numbers. The order depends on what
`display-line-numbers-type' is set to. If you're using Emacs 26+, and
visual-line-mode is on, this skips relative and uses visual instead.

See `display-line-numbers' for what these values mean."
    (interactive)
    (defvar kei--line-number-style display-line-numbers-type)
    (let* ((styles `(t ,(if visual-line-mode 'visual 'relative) nil))
           (order (cons display-line-numbers-type (remq display-line-numbers-type styles)))
           (queue (memq kei--line-number-style order))
           (next (if (= (length queue) 1)
                     (car order)
                   (car (cdr queue)))))
      (setq kei--line-number-style next)
      (setq display-line-numbers next)
      (message "Switched to %s line numbers"
               (pcase next
                 (`t "normal")
                 (`nil "disabled")
                 (_ (symbol-name next))))))

  :bind
  (("C-c t l" . kei/toggle-line-numbers)
   ("C-c o u" . insert-char)
   :map global-map
   ("<C-left>" . enlarge-window-horizontally)
   ("<C-down>" . shrink-window)
   ("<C-up>" . enlarge-window)
   ("<C-right>" . shrink-window-horizontally)))

;;; Isearch
(use-package isearch
  :demand
  :config
  (defun kei/search-isearch-abort-dwim ()
    "Delete failed `isearch' input, single char, or cancel search.

This is a modified variant of `isearch-abort' that allows us to
perform the following, based on the specifics of the case: (i)
delete the entirety of a non-matching part, when present; (ii)
delete a single character, when possible; (iii) exit current
search if no character is present and go back to point where the
search started."
    (interactive)
    (if (eq (length isearch-string) 0)
        (isearch-cancel)
      (isearch-del-char)
      (while (or (not isearch-success) isearch-error)
        (isearch-pop-state)))
    (isearch-update))

  :bind
  (:map isearch-mode-map
        ("<backspace>". kei/search-isearch-abort-dwim)))

;;; list-buffer->ibuffer
(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

(use-package ibuffer-projectile
  :ensure t
  :hook
  (ibuffer-hook . (lambda ()
                    (progn
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic))))))

;;; Files
(use-package files
  :hook
  (before-save . delete-trailing-whitespace)

  :custom
  (require-final-newline t)
  ;; backup settings
  (backup-by-copying t)
  ;; REVIEW (no-littering)
  ;; (backup-directory-alist
  ;;  `((".*" . ,(locate-user-emacs-file "tmp/backups/"))))
  ;; (auto-save-list-file-prefix (locate-user-emacs-file "tmp/auto-saves/sessions/"))
  ;; (auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "tmp/auto-saves/") t)))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (confirm-kill-emacs #'yes-or-no-p)
  :config

  (defun kei/find-file-in-org ()
    "Search for a file in `org'."
    (interactive)
    (projectile-find-file-in-directory "~/org"))

  (defun kei/find-file-in-keimacs()
    "Search for a file in `org'."
    (interactive)
    (projectile-find-file-in-directory "~/.config/keimacs"))

  (defun kei/find-config-in-nix ()
    "Search file in private Doom config in nix config."
    (interactive)
    (sudo-edit (projectile-find-file-in-directory "/etc/nixos/config/emacs/doom/")))

  :bind
  (("C-c o o" . kei/find-file-in-org)
   ("C-c o c" . kei/find-file-in-keimacs)
   ("C-c o p" . kei/find-config-in-nix)))

(use-package no-littering
  :ensure t
  :custom
  (auto-save-file-name-transforms .
                                  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package recentf
  :defer 0.1
  :custom
  (recentf-auto-cleanup 30)
  :config
  (recentf-mode)
  (run-with-idle-timer 10 t 'recentf-save-list)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package autorevert
  :defer 0.1)

;;; Quick access to init file
(use-package iqa
  :ensure t
  :config
  (iqa-setup-default))

;;; custom file -> /dev/null
(use-package cus-edit
  :defer t
  :custom
  (custom-file null-device "Don't store customizations"))

;;; Edit as sudo
(use-package tramp
  :defer t
  :config
  (put 'temporary-file-directory 'standard-value `(,temporary-file-directory))
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil))

(use-package sudo-edit
  :ensure t
  :config (sudo-edit-indicator-mode)
  :bind (:map ctl-x-map
              ("M-s" . sudo-edit)))

;;; Dired
(use-package dired
  :commands dired-jump
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  (dired-auto-revert-buffer t))

(use-package dired-aux
  :after dired
  :custom
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t))

(use-package ranger
  :ensure t
  :after dired
  :custom
  (ranger-override-dired-mode t)
  (ranger-override-dired t)
  (ranger-cleanup-on-disable t)
  (ranger-deer-show-details t)
  (ranger-show-hidden nil)
  (ranger-footer-delay nil))

(use-package dired-async
  :hook
  (dired-mode-hook . dired-async-mode))

;;; Subr
(use-package subr
  :defer t
  :preface
  (provide 'subr)
  :init
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;;; Startup
(use-package startup
  :defer t
  :preface
  (provide 'startup)
  :custom
  (user-mail-address "druksasha@ukr.net")
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message t)
  (initial-major-mode #'org-mode))

;;; Scratch per major-mode
(use-package scratch
  :ensure
  :config
  (defun prot/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly."
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t)))
  :hook (scratch-create-buffer-hook . prot/scratch-buffer-setup)
  :bind ("C-c s" . scratch))

;;; Winner
(use-package winner
  :config
  (winner-mode 1))

;;; Theme
(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-completions 'moderate)
  (modus-themes-mode-line 'borderless)
  (modus-themes-region 'accent-no-extend)
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-variable-pitch-headings t)
  (modus-themes-lang-checkers nil)
  (modus-themes-paren-match 'intense)
  (modus-themes-hl-line 'accented-background)
  (modus-themes-slanted-constructs t)
  :init
  ;; (load-theme 'modus-operandi t))
  (load-theme 'modus-vivendi t))

;;; Solaire-mode
;; REVIEW
;; (use-package solaire-mode
;;   :ensure t
;;   :config
;;   (solaire-global-mode +1))

;;; Faces
;; TODO
(use-package faces
  :defer t
  :bind
  (("C-=" . text-scale-increase)
   ("C-_" . text-scale-decrease))
  :custom-face
  (default ((t (:font "Iosevka"))))
  (fixed-pitch ((t (:font "Iosevka"))))
  (variable-pitch ((t (:font "Iosevka")))))

(use-package unicode-fonts
  :ensure t
  :init
  (unicode-fonts-setup))

;;; Cursor
(use-package frame
  :defer t
  :custom
  (blink-cursor-mode nil))

;;; Evil
(use-package evil
  :ensure t
  :custom
  (evil-want-Y-yank-to-eol t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-disable-insert-state-bindings t)
  ;; (evil-echo-state nil)
  (evil-undo-system 'undo-fu)
  (evil-search-module 'evil-search)
  (evil-respect-visual-line-mode t)
  :hook
  (after-init-hook . evil-mode)
  :config
  (evil-mode)
  (evil-escape-mode)
  ;; (evil-set-leader 'motion (kbd "SPC"))
  ;; (evil-set-leader 'motion (kbd ",") t)

  (defun custom-evil-force-normal-state ()
    "Delegate to evil-force-normal-state but also clear search highlighting"
    (interactive)
    (progn
      (keyboard-escape-quit)
      (evil-force-normal-state)
      (evil-ex-nohighlight)))

;;; Maximize window
  (defun toggle-maximize-buffer () "Maximize buffer"
         (interactive)
         (if (= 1 (length (window-list)))
             (jump-to-register '_)
           (progn
             (window-configuration-to-register '_)
             (delete-other-windows))))


  :bind
  ( ; Is there a better way? (maybe use doom advice)
   :map evil-motion-state-map
   ("C-w Q" . kill-buffer-and-window)
   ("<escape>" . custom-evil-force-normal-state)
   ([tab] . evil-jump-item)
   :map evil-normal-state-map
   ("<escape>" . custom-evil-force-normal-state)
   :map evil-insert-state-map
   ("<escape>" . custom-evil-force-normal-state)
   :map evil-window-map
   ("<escape>" . custom-evil-force-normal-state)
   ("m" . toggle-maximize-buffer)
   :map evil-operator-state-map
   ("<escape>" . custom-evil-force-normal-state)))

(use-package evil-collection
  :ensure t
  :hook
  (evil-mode-hook . evil-collection-init))

;;; Format all the code
;;; FIXME
(use-package format-all
  :ensure t
  :init
  (add-hook 'before-save-hook (lambda () (call-interactively #'format-all-buffer)))
  ;; :hook
  ;; (prog-mode-hook . format-all-mode)
  ;; (prog-mode-hook . (lambda ()
  ;;                       (add-hook 'before-save-hook #'format-all-buffer)))
  ;; (before-save-hook . #'format-all-buffer)
  )


;;; Evil-commentary
(use-package evil-commentary
  :ensure t
  :bind
  (:map evil-motion-state-map
        ("gC" . evil-commentary-yank))
  :hook
  (evil-mode-hook . evil-commentary-mode))

;;; Evil-surround
(use-package evil-surround
  :ensure t
  :hook
  (evil-local-mode-hook . evil-surround-mode))


;;; Evil-multiedit
(use-package evil-multiedit
  :defer 0.2
  :ensure t
  :bind
  (:map evil-motion-state-map
        ("M-d" . evil-multiedit-match-symbol-and-next)
        ("M-D" . evil-multiedit-match-symbol-and-prev)
        ("C-M-d" . evil-multiedit-restore)))

;; Smartparens
(use-package smartparens
  :hook
  (prog-mode-hook . smartparens-global-mode)
  (prog-mode-hook . show-smartparens-mode)
  :config
  (sp-pair "`" nil :actions nil)
  (sp-pair "'" nil :actions nil))

;;; Evil-cleverparens
(use-package evil-cleverparens
  :ensure t
  :preface
  (defun do-not-map-M-s-and-M-d (f)
    (let ((evil-cp-additional-bindings
           (progn
             (assoc-delete-all "M-s" evil-cp-additional-bindings)
             (assoc-delete-all "M-D" evil-cp-additional-bindings)
             (assoc-delete-all "M-d" evil-cp-additional-bindings))))
      (funcall f)))
  :custom
  (evil-cleverparens-use-s-and-S nil)
  :hook
  (emacs-lisp-mode-hook . evil-cleverparens-mode)
  (lisp-mode-hook . evil-cleverparens-mode)
  (scheme-mode-hook . evil-cleverparens-mode)
  (clojure-mode-hook . evil-cleverparens-mode)
  (clojurec-mode-hook . evil-cleverparens-mode)
  (clojurescript-mode-hook . evil-cleverparens-mode)
  (lisp-interaction-mode-hook . evil-cleverparens-mode)
  :init
  (advice-add 'evil-cp-set-additional-bindings :around #'do-not-map-M-s-and-M-d))

(use-package undo-fu
  :ensure t)

;; jj --> esc
(use-package evil-escape
  :ensure t
  :config
  (evil-define-command kei/maybe-exit ()
    :repeat change
    (interactive)
    (let ((modified (buffer-modified-p)))
      (insert "о")
      (let ((evt (read-event nil 0.9)))
        (cond
         ((null evt) (message ""))
         ((and (integerp evt) (char-equal evt ?о))
	      (delete-char -1)
	      (set-buffer-modified-p modified)
	      (push 'escape unread-command-events))
         (t (setq unread-command-events (append unread-command-events
					                            (list evt))))))))
  :bind
  ;; oo --> esc
  (:map evil-insert-state-map
        ("о" . kei/maybe-exit))
  :custom
  (evil-escape-key-sequence "jj")
  (evil-escape-delay 0.2)
  (evil-escape-excluded-states '(normal
                                 visual
                                 multiedit
                                 emacs
                                 motion)))

;; (define-key evil-insert-state-map "о" #'kei/maybe-exit)


;; Treat underscore as a part of a word in code
;; TODO
(add-hook 'prog-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'prog-mode-hook
          (lambda () (modify-syntax-entry ?- "w")))
;; Fancy lambdas
(global-prettify-symbols-mode t)


;;; Which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; Helpful
(use-package helpful
  :ensure t
  :defer t)


;;; Reverse-im
(use-package reverse-im
  :ensure t
  :init
  :custom (reverse-im-input-methods '("russian-computer"))
  :config (reverse-im-mode t))

;; Localization
(use-package mule
  :defer 0.1
  :config
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-terminal-coding-system 'utf-8))

(use-package ispell
  :defer t
  :custom
  (ispell-local-dictionary-alist
   '(("russian"
      "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
      "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
      "[-']"  nil ("-d" "uk_UA,ru_RU,en_US") nil utf-8)))
  (ispell-program-name "aspell")
  (ispell-dictionary "english")
  (ispell-really-aspell t)
  (ispell-really-hunspell nil)
  (ispell-encoding8-command t)
  (ispell-silently-savep t))

(use-package flyspell
  :defer t
  :custom
  (flyspell-delay 1))

(use-package flyspell-correct-popup
  :ensure t
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-at-point)))

;; Dashboard
;; TODO setup projectile
(use-package dashboard
  :ensure t
  :preface
  (defun kei/dashboard-banner ()
    "Set a dashboard banner including information on package initialization
			   time and garbage collections."""
    (setq dashboard-banner-logo-title
	      (format "Emacs ready in %.2f seconds with %d garbage collections."
		          (float-time (time-subtract after-init-time before-init-time)) gcs-done)))
  :custom
  (dashboard-startup-banner "~/pix/doom/stallman.png")
  (dashboard-center-content t)
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     ;; (projects . 5)
                     (agenda . 5)
                     (registers . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-refresh-buffer))

;;; Fancy
(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 95))

(use-package font-lock+
  :defer t
  :quelpa
  (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

(use-package all-the-icons
  :ensure t
  :defer t
  :config
  (setq all-the-icons-mode-icon-alist
        `(,@all-the-icons-mode-icon-alist
          (package-menu-mode all-the-icons-octicon "package" :v-adjust 0.0)
          (telega-chat-mode all-the-icons-fileicon "telegram" :v-adjust 0.0
                            :face all-the-icons-blue-alt)
          (telega-root-mode all-the-icons-material "contacts" :v-adjust 0.0))))

(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode-hook . all-the-icons-dired-mode))

(use-package emojify
  :ensure t
  :hook
  (after-init-hook . global-emojify-mode))

;;; Modeline
(use-package doom-modeline
  :ensure t
  :custom

  (doom-modeline-height 15)
  ;; (doom-modeline-enable-word-count t)
  :init
  (doom-modeline-mode 1)
  (display-time-mode 1)) ; Clock in modeline

(use-package anzu
  :ensure t
  :hook
  (after-init-hook . global-anzu-mode))

(use-package evil-anzu
  :ensure t)

;;; Imenu
(use-package imenu
  :bind
  (:map goto-map
        ("i" . imenu))
  :custom
  (imenu-auto-rescan t)
  (imenu-auto-rescan-maxout 60000)
  (imenu-use-popup-menu nil)
  (imenu-eager-completion-buffer t)
  :hook
  (imenu-after-jump-hook . recenter-top-bottom))

(use-package flimenu
  :ensure t
  :hook
  (after-init-hook . flimenu-global-mode))

;;; Kill-buffer
(use-package menu-bar
  :bind
  (:map ctl-x-map
        ("K" . kill-this-buffer)))

;;; Minibuffer
(use-package minibuffer
  :custom
  (completion-cycle-threshold 1)
  (completion-styles '(partial-completion orderless))
  (completion-show-help nil)
  :bind
  ;; TODO change binding
  ("C-SPC" . completion-at-point))

(use-package minibuffer-eldef
  :hook
  (after-init-hook . minibuffer-electric-default-mode))

(use-package mb-depth
  :ensure t
  :hook
  (after-init-hook . minibuffer-depth-indicate-mode))

;;; Saveplace
(use-package saveplace
  :ensure t
  :hook
  (after-init-hook . save-place-mode))

;;; Savehist
(use-package savehist
  :custom
  (history-delete-duplicates t)
  :hook
  (after-init-hook . savehist-mode))


;;;; Emacs Lisp
(use-package elisp-mode
  :custom
  (eval-expression-print-level t)
  (eval-expression-print-length t)
  :bind
  (:map emacs-lisp-mode-map
        ("C-c e e" . eval-last-sexp)
        ("C-c e b" . eval-buffer)
        ("C-c e d" . eval-defun)
        ("C-c e p" . eval-print-last-sexp)
        ("C-c e r" . eval-region)
        ("C-c d d" . describe-function)))

(use-package eros
  :ensure t
  :hook
  (emacs-lisp-mode-hook . eros-mode))

(use-package suggest
  :ensure t
  :defer t)

(use-package ipretty
  :defer t
  :ensure t
  :config
  (ipretty-mode 1))

;;;; Scheme
(use-package scheme
  :bind
  (:map scheme-mode-map
        ("C-c c c" . geiser-connect)
        ("C-c c r" . run-geiser)))

(use-package geiser-mode
  :custom
  (geiser-active-implementations '(guile))
  :bind
  (:map geiser-mode-map
        ("C-C e e" . geiser-eval-last-sexp)
        ("C-C e d" . geiser-eval-definition)
        ("C-C e b" . geiser-eval-buffer)
        ("C-C m e" . geiser-expand-last-sexp)
        ("C-C m d" . geiser-expand-definition)
        ("C-C m r" . geiser-expand-region)
        ("C-C y"   . geiser-insert-lambda)
        ("C-C r b" . switch-to-geiser)
        ("C-C r q" . geiser-repl-exit)
        ("C-C r c" . geiser-repl-clear-buffer)))

;; REVIEW Do I really need this?
(use-package geiser-guile
  :ensure t)

;;; TODO https://git.sr.ht/~sokolov/geiser-eros
;; (use-package geiser-eros
;;   :hook
;;   (geiser-mode-hook . geiser-eros-mode))


;;; Icomplete
(use-package icomplete
  :after minibuffer
  :custom
  (icomplete-in-buffer t)
  (icomplete-delay-completions-threshold 0)
  (icomplete-max-delay-chars 0)
  (icomplete-compute-delay 0)
  (icomplete-show-matches-on-no-input t)
  (icomplete-show-common-prefix nil)
  (icomplete-prospects-height 10)
  :bind
  (:map icomplete-minibuffer-map
        ("C-j" . icomplete-forward-completions)
        ("C-n" . icomplete-forward-completions)
        ("C-k" . icomplete-backward-completions)
        ("C-p" . icomplete-backward-completions)
        ("C-l" . icomplete-force-complete)
        ("<return>" . icomplete-force-complete-and-exit)
        ("<tab>" . icomplete-force-complete)
        ("C-u" . kill-whole-line)
        ("<backspace>" . kei/minibuffer-backward-updir))
  :config
  (defun kei/minibuffer-backward-updir ()
    "Delete char before point or go up a directory.
Must be bound to `minibuffer-local-filename-completion-map'."
    (interactive)
    (if (and (eq (char-before) ?/)
             (eq (kei/minibuffer--completion-category) 'file))
        (save-excursion
          (goto-char (1- (point)))
          (when (search-backward "/" (point-min) t)
            (delete-region (1+ (point)) (point-max))))
      (call-interactively 'backward-delete-char)))

  (defun kei/minibuffer--completion-category ()
    "Return completion category."
    (let* ((beg (kei/minibuffer--field-beg))
           (md (completion--field-metadata beg)))
      (alist-get 'category (cdr md))))

  (defun kei/minibuffer--field-beg ()
    "Determine beginning of completion."
    (if (window-minibuffer-p)
        (minibuffer-prompt-end)
      (nth 0 completion-in-region--data)))

  :hook
  (after-init-hook . icomplete-mode))

(use-package icomplete-vertical
  :ensure t
  :bind
  (:map icomplete-minibuffer-map
        ("C-t" . icomplete-vertical-toggle))
  :hook
  (icomplete-mode-hook . icomplete-vertical-mode))

;;; Marginalia
(use-package marginalia
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light))
  :hook
  (after-init-hook . marginalia-mode))

;;; Orderless
(use-package orderless
  :ensure t
  :after icomplete
  :preface
  (defun orderless-literal-dispatcher (p _ _)
    (when (string-prefix-p "=" p)
      `(orderless-literal . ,(substring p 1))))
  (defun orderless-sli-dispatcher (p _ _)
    (when (string-prefix-p "-" p)
      `(orderless-strict-leading-initialism . ,(substring p 1))))
  :custom
  ;; REVIEW orderless-literal
  ;; (orderless-matching-styles '(orderless-flex orderless-regexp orderless-literal))
  (orderless-matching-styles '(orderless-flex orderless-regexp))
  (orderless-style-dispatchers '(orderless-literal-dispatcher
                                 orderless-sli-dispatcher)))

;;; Consult
;;; REVIEW
(use-package consult
  :ensure t
  :custom
  (completion-in-region-function 'consult-completion-in-region)
  :bind
  (:map goto-map
        ("o" . consult-outline)
        ("i" . consult-imenu)
        ("E" . consult-compile-error)
        ("f" . consult-flymake))
  :config
  (setf (alist-get #'consult-completion-in-region consult-config)
        '(:completion-styles (basic))))

;;; Pdf
(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-display-size 'fit-height)
  :config
  (pdf-tools-install))

(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :hook
  (pdf-view-mode-hook . pdf-view-restore-mode))

;;;; Highlighting
(use-package hl-line
  :ensure t
  :config
  (global-hl-line-mode 1))

(use-package page-break-lines
  :ensure t
  :hook
  (help-mode-hook . page-break-lines-mode)
  (prog-mode-hook . page-break-lines-mode)
  (special-mode . page-break-lines-mode)
  (compilation-mode . page-break-lines-mode))

(use-package rainbow-mode
  :ensure t
  :hook '(prog-mode-hook help-mode-hook))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(use-package evil-goggles
  :ensure t
  :custom
  (evil-goggles-duration 0.150)
  (evil-goggles-enable-delete nil)
  :config
  (evil-goggles-mode)
  ;; REVIEW
  (evil-goggles-use-diff-faces))

(use-package hl-todo
  :ensure t
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode-hook . hl-todo-mode)
         (yaml-mode-hook . hl-todo-mode)))

(use-package so-long
  :ensure t
  :hook
  (after-init-hook . global-so-long-mode))

;;;; Projectile
(use-package projectile
  :defer 0.2
  :ensure t
  :init (projectile-mode +1)
  :bind
  (:map mode-specific-map ("p" . projectile-command-map))
  :custom
  (projectile-project-search-path (cddr (directory-files "~/code/" t)))
  (projectile-require-project-root nil)
  (projectile-project-root-files-functions
   '(projectile-root-local
     projectile-root-top-down
     projectile-root-bottom-up
     projectile-root-top-down-recurring))
  (projectile-completion-system 'default))


;;;; Autocompletion
;;; Company
(use-package company
  :ensure t
  :custom
  (company-backends '((company-capf company-dabbrev-code company-files)))
  :bind
  (:map company-active-map
        ("C-j" . company-select-next)
        ("C-n" . company-select-next)
        ("C-k" . company-select-previous)
        ("C-p" . company-select-previous)
        ("C-l" . company-complete-selection)
        ;; ("RET" . company-complete-selection)
        ([tab] . company-complete-common-or-cycle))
  :hook
  (after-init-hook . global-company-mode))

;;; Vterm
(use-package vterm
  :ensure t
  :config
  (setq vterm-buffer-name "vterm")
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3)))

  (defun +vterm/toggle (arg)
    "Toggles a terminal popup window at project root.

If prefix ARG is non-nil, recreate vterm buffer in the current project's root.

Returns the vterm buffer."
    (interactive "P")
    (+vterm--configure-project-root-and-display
     arg
     (lambda()
       (let ((buffer-name
              (format "*vterm-popup:%s*"
                      (if (bound-and-true-p persp-mode)
                          (safe-persp-name (get-current-persp))
                        "main")))
             confirm-kill-processes
             current-prefix-arg)
         (when arg
           (let ((buffer (get-buffer buffer-name))
                 (window (get-buffer-window buffer-name)))
             (when (buffer-live-p buffer)
               (kill-buffer buffer))
             (when (window-live-p window)
               (delete-window window))))
         (if-let (win (get-buffer-window buffer-name))
             (if (eq (selected-window) win)
                 (delete-window win)
               (select-window win)
               (when (bound-and-true-p evil-local-mode)
                 (evil-change-to-initial-state))
               (goto-char (point-max)))
           (let ((buffer (get-buffer-create buffer-name)))
             (with-current-buffer buffer
               (unless (eq major-mode 'vterm-mode)
                 (vterm-mode))
               (+vterm--change-directory-if-remote))
             (pop-to-buffer buffer)))
         (get-buffer buffer-name)))))

  (defun +vterm/here (arg)
    "Open a terminal buffer in the current window at project root.

If prefix ARG is non-nil, cd into `default-directory' instead of project root.

Returns the vterm buffer."
    (interactive "P")
    (+vterm--configure-project-root-and-display
     arg
     (lambda()
       (require 'vterm)
       ;; HACK forces vterm to redraw, fixing strange artefacting in the tty.
       (save-window-excursion
         (pop-to-buffer "*scratch*"))
       (let (display-buffer-alist)
         (vterm vterm-buffer-name)))))

  (defun +vterm--configure-project-root-and-display (arg display-fn)
    "Sets the environment variable PROOT and displays a terminal using `display-fn`.

If prefix ARG is non-nil, cd into `default-directory' instead of project root.

Returns the vterm buffer."
    (unless (fboundp 'module-load)
      (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
    (let* ((project-root default-directory)
           (default-directory
             (if arg
                 default-directory
               project-root)))
      (setenv "PROOT" project-root)
      (prog1 (funcall display-fn)
        (+vterm--change-directory-if-remote))))

  (defun +vterm--change-directory-if-remote ()
    "When `default-directory` is remote, use the corresponding
method to prepare vterm at the corresponding remote directory."
    (when (and (featurep 'tramp)
               (tramp-tramp-file-p default-directory))
      (message "default-directory is %s" default-directory)
      (with-parsed-tramp-file-name default-directory path
        (let ((method (cadr (assoc `tramp-login-program
                                   (assoc path-method tramp-methods)))))
          (vterm-send-string
           (concat method " "
                   (when path-user (concat path-user "@")) path-host))
          (vterm-send-return)
          (vterm-send-string
           (concat "cd " path-localname))
          (vterm-send-return)))))
  :bind
  (:map evil-motion-state-map
        ("C-c o V" . +vterm/here)
        ("C-c o v" . +vterm/toggle)))

;; (use-package vterm-toggle
;;   :ensure t)



;; REVIEW
;; (use-package vterm-toggle
;;   :ensure t
;;   :custom
;;   (vterm-toggle-fullscreen-p . nil)
;;   :config
;;   (add-to-list 'display-buffer-alist
;;                '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
;;                  (display-buffer-reuse-window display-buffer-at-bottom)
;;                  (reusable-frames . visible)
;;                  (window-height . 0.3))))


;;;; Quick jumps
;;; Avy
(use-package avy
  :ensure t
  :bind
  (:map evil-motion-state-map
        ("g s" .   avy-goto-char-timer)
        :map goto-map
        ("M-g" . avy-goto-line)
        :map search-map
        ("M-s" . avy-goto-word-1)))

(use-package link-hint
  :ensure t
  :bind
  (:map mode-specific-map
        :prefix-map link-hint-keymap
        :prefix "l"
        ("o" . link-hint-open-link)
        ("c" . link-hint-copy-link)))

(use-package frog-jump-buffer
  :ensure t
  :bind
  (:map goto-map
        ("b" . frog-jump-buffer)))

;;;; Lookup

;;; Google-this
(use-package google-this
  :defer 0.1
  :ensure t
  :bind
  (:map mode-specific-map
        ("g" . google-this-mode-submap)))

;;; Google-translate
(use-package google-translate
  :ensure t
  :commands (google-translate-smooth-translate)
  :config
  (defun kei/google-translate-at-point (arg)
    "Translate word at point. If prefix is provided, do reverse translation"
    (interactive "P")
    (if arg
	    (google-translate-at-point-reverse)
      (google-translate-at-point)))

  (require 'google-translate-default-ui)
  (require 'google-translate-smooth-ui)
  (setq google-translate-show-phonetic t)

  ;; (setq google-translate-default-source-language "en"
  ;;       google-translate-default-target-language "ru")

  (setq google-translate-translation-directions-alist '(("en" . "ru") ("ru" . "en") ("en" . "uk") ("uk" . "en")))

  ;; auto-toggle input method
  (setq google-translate-input-method-auto-toggling t
	    google-translate-preferable-input-methods-alist '((nil . ("en"))
							                              (russian-computer . ("ru"))))
  :bind
  (:map evil-motion-state-map
        ("C-c o t" . google-translate-smooth-translate)
        ("C-c o T" . kei/google-translate-at-point)))

;;;; FIXME find a better way to define this?
(require 'google-translate)
(defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
(setq google-translate-backend-method 'curl)

;; Google-translate dependency
(use-package popup
  :ensure t)

;;; TODO export to sh-mode
;; When saving a file that start with '#!', make it executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;;; Multiple cursors
;;; REVIEW
;; (use-package evil-mc
;;   :ensure t
;;   :config
;;   :bind
;;   (:map evil-motion-state-map
;;         ("g b u" . evil-mc-undo-cursor)
;;         ("g b z" . evil-mc-toggle-cursor-here)
;;         ("g b t" . evil-mc-toggle-cursors)))

;;; Tabs
(use-package tab-bar
  :custom
  (tab-bar-close-button-show . nil)
  (tab-bar-position . nil)
  (tab-bar-show . nil)
  (tab-bar-tab-hints . nil)
  :config
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)
  (tab-bar-mode -1)
  (tab-bar-history-mode -1)

  (defun kei/tab-bar-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (icomplete-vertical-do ()
               (tab-bar-switch-to-tab
                (completing-read "Select tab: " tabs nil t)))))))

  :bind (("C-x t t" . kei/tab-bar-select-tab-dwim)
         ("C-x t n" . tab-new)
         ("C-x t q" . tab-close)
         ("C-x t s" . tab-switcher)))
