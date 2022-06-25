;;; -*- lexical-binding: t; -*-

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
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents)               ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

;; Setup use-package
(eval-when-compile
  (require 'use-package))

;;; quelpa
(use-package quelpa
  :ensure t
  :defer t
  :init
  (defun quelpa-build--checkout-sourcehut (name config dir)
    "Check package NAME with config CONFIG out of sourcehut into DIR."
    (let ((url (format "https://git.sr.ht/~%s" (plist-get config :repo))))
      (quelpa-build--checkout-git name (plist-put (copy-sequence config) :url url) dir)))

  (defun quelpa-build--checkout-sourcehut-ssh (name config dir)
    "Check package NAME with config CONFIG out of sourcehut into DIR."
    (let ((url (format "git@git.sr.ht:~%s" (plist-get config :repo))))
      (quelpa-build--checkout-git name (plist-put (copy-sequence config) :url url) dir)))
  :custom
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

;;; use-package-quelpa
(use-package quelpa-use-package
  :ensure t
  :init (setq quelpa-use-package-inhibit-loading-quelpa t))

;;; use-package custom-update
;; To be able update lists in custom
(use-package use-package-custom-update
  :ensure t
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
  :ensure t
  :quelpa
  (fnhh :repo "a13/fnhh" :fetcher github)
  :config
  (fnhh-mode 1))

;;; Modernized package menu
(use-package paradox
  :ensure t
  :defer 1
  :config
  (paradox-enable))

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
  (scroll-conservatively 101)
  (scroll-step 1)
  (scroll-margin)
  ;; (truncate-lines t)
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

  ;; emacs 28.1
  (completions-detailed t)
  (next-error-message-highlight t)
  (help-enable-symbol-autoload t)
  (describe-bindings-outline t)

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
  (("C-c k n" . kei/toggle-line-numbers)
   ("C-c k u" . insert-char)
   ("C-c k f" . make-frame)
   ("C-c k z" . olivetti-mode)
   :map global-map
   ("<C-left>" . enlarge-window-horizontally)
   ("<C-down>" . shrink-window)
   ("<C-up>" . enlarge-window)
   ("<C-right>" . shrink-window-horizontally))
  :hook
  ;; Treat underscore as a part of a word in code FIXME
  ((prog-mode-hook
    cider-repl-mode-hook
    sly-mrepl-hook) . (lambda () (progn
                              (modify-syntax-entry ?- "w")
                              (modify-syntax-entry ?_ "w")
                              (modify-syntax-entry ?* "w")
                              (modify-syntax-entry ?? "w")
                              (modify-syntax-entry ?! "w")
                              (modify-syntax-entry ?> "w")))))

;;; Isearch
(use-package isearch
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

;;; Ripgrep
(use-package rg
  :ensure t
  :init
  (rg-enable-default-bindings)
  :bind
  (:map search-map
        ("g" . rg))
  :init
  (advice-add 'project-find-regexp :override #'rg-project))

;;; Files
(use-package files
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (require-final-newline nil)
  ;; backup settings
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (confirm-kill-emacs #'yes-or-no-p)
  :config
  (defun kei/find-file-in-org ()
    "Search for a file in `org'."
    (interactive)
    (counsel-find-file "" "~/org/"))

  (defun kei/find-file-in-keimacs()
    "Search for a file in `emacs'."
    (interactive)
    (counsel-find-file "" "~/.config/emacs/"))

  (defun kei/find-file-in-nix()
    "Search for a file in `nixos'."
    (interactive)
    (counsel-find-file "" "~/nix/nixos-config/"))

  :bind
  (:map global-map
        ("C-c f o" . kei/find-file-in-org)
        ("C-c f k" . kei/find-file-in-keimacs)
        ("C-c f p" . kei/find-file-in-nix)
        ("C-c f m" . rename-file)
        ("C-c f D" . delete-file)
        ("C-c f c" . copy-file)
        ("C-c f s" . sudo-edit)))

(use-package no-littering
  :ensure t
  :custom
  (auto-save-file-name-transforms .
                                  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package recentf
  ;; :disabled
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
  ;; :config
  ;; (put 'temporary-file-directory 'standard-value `(,temporary-file-directory))
  :custom
  ;; (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil))

(use-package sudo-edit
  :ensure t
  :config (sudo-edit-indicator-mode)
  :bind (:map ctl-x-map
              ("M-s" . sudo-edit)
              ("M-f" . sudo-edit-find-file)))

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
  :custom
  (use-short-answers t)) ; emacs 28.1

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
  (initial-major-mode #'emacs-lisp-mode))

;;; Scratch per major-mode
(use-package scratch
  :ensure
  :config
  (defun kei/scratch-buffer-setup ()
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
  :hook (scratch-create-buffer-hook . kei/scratch-buffer-setup)
  :bind ("C-c k s" . scratch))

;;; Winner
(use-package winner
  :config
  (winner-mode 1))


;;; Theme
(let ((theme-file "~/.config/emacs/current-theme.el"))
  (when (file-exists-p theme-file)
    (load-file theme-file)))


;;; Faces
;; TODO
(use-package faces
  :defer t
  :bind
  (("C-=" . text-scale-increase)
   ("C-_" . text-scale-decrease))
  :custom-face
  (default ((t (:font "Iosevka" :height 100))))
  (fixed-pitch ((t (:font "Iosevka" :height 100))))
  (variable-pitch ((t (:font "Iosevka" :height 100))))
  (mode-line ((t (:font "Iosevka" :height 100))))
  (mode-line-inactive ((t (:font "Iosevka" :height 100)))))

(use-package unicode-fonts
  :ensure t
  :init
  (unicode-fonts-setup))

;;; Cursor
(use-package frame
  :defer t
  :custom
  (cursor-type '(hbar . 1))
  (blink-cursor-mode t))

(use-package beacon
  :ensure t
  :bind
  (("C-x j" . beacon-blink))
  :hook
  (after-init-hook . beacon-mode))

;;; Evil
(use-package evil
  :ensure t
  :custom
  ;; (evil-want-Y-yank-to-eol t)
  ;; (evil-want-integration t)
  (evil-want-keybinding nil)
  ;; (evil-split-window-below t)
  ;; (evil-vsplit-window-right t)
  ;; (evil-disable-insert-state-bindings t)
  ;; (evil-want-C-i-jump nil)
  ;; ;; (evil-echo-state nil)
  (evil-undo-system 'undo-fu)
  ;; (evil-search-module 'evil-search)
  ;; (evil-respect-visual-line-mode t)
  :hook
  (after-init-hook . evil-mode)
  :config
  (evil-mode)
  (evil-escape-mode)
  (setq evil-insert-state-cursor '(bar . 1))
  (setq evil-emacs-state-cursor '(hbar . 1))
  (setq evil-normal-state-cursor '(hbar . 1))
  (setq evil-visual-state-cursor '(hbar . 2))
  (setq evil-motion-state-cursor '(hbar . 2))
  (setq evil-replace-state-cursor '(hbar . 2))
  (setq evil-operator-state-cursor '(hbar . 2))

  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-integration t)
  ;; (setq evil-want-keybinding nil)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-C-i-jump nil)
  (setq evil-search-module 'evil-search)
  (setq evil-respect-visual-line-mode t)

  (defun custom-evil-force-normal-state ()
    "Delegate to evil-force-normal-state but also clear search highlighting"
    (interactive)
    (progn
      ;; (keyboard-escape-quit)
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
   ;; ([tab] . evil-jump-item)
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
;;; TODO format-all-formatters (("Nix" nixfmt) ("C" . clang) ...)
(use-package format-all
  :ensure t
  :bind
  (("C-c f f" . format-all-buffer))
  :init
  ;; (add-hook 'prog-mode-hook 'format-all-mode)
  ;; (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
  ;; (add-hook 'before-save-hook (lambda () (call-interactively #'format-all-buffer)))
  :hook
  (prog-mode-hook . format-all-mode)
  ;; (format-all-mode-hook . format-all-ensure-formatter)
  ;; (prog-mode-hook . (lambda ()
  ;;                       (add-hook 'before-save-hook #'format-all-buffer)))
  ;; (before-save-hook . #'format-all-buffer)
  )


;;; Evil-commentary
(use-package evil-commentary
  :ensure t
  :bind
  (:map evil-motion-state-map
        ("g C" . evil-commentary-yank))
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
  ;; :custom
  ;; (evil-digit-bound-motions '(evil-beginning-of-visual-line))
  :bind
  (:map evil-motion-state-map
		("M-d" . evil-multiedit-match-symbol-and-next)
		("M-D" . evil-multiedit-match-symbol-and-prev)
		("M-R" . evil-multiedit-match-all)
		("C-M-d" . evil-multiedit-restore)))

;; Smartparens
(use-package smartparens
  :hook
  (after-init-hook . smartparens-global-mode)
  (after-init-hook . show-smartparens-global-mode)
  :config
  (sp-pair "`" nil :actions nil)
  (sp-pair "'" nil :actions nil)

  (defun radian-enter-and-indent-sexp (&rest _ignored)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (dolist (mode '(c-mode c++-mode css-mode objc-mode java-mode
                         js2-mode json-mode
                         python-mode sh-mode web-mode))
    (sp-local-pair mode "{" nil :post-handlers
                   '((radian-enter-and-indent-sexp "RET")
                     (radian-enter-and-indent-sexp "<return>"))))

  (dolist (mode '(js2-mode json-mode python-mode web-mode))
    (sp-local-pair mode "[" nil :post-handlers
                   '((radian-enter-and-indent-sexp "RET")
                     (radian-enter-and-indent-sexp "<return>"))))

  (dolist (mode '(python-mode))
    (sp-local-pair mode "(" nil :post-handlers
                   '((radian-enter-and-indent-sexp "RET")
                     (radian-enter-and-indent-sexp "<return>")))
    (sp-local-pair mode "\"\"\"" "\"\"\"" :post-handlers
                   '((radian-enter-and-indent-sexp "RET")
                     (radian-enter-and-indent-sexp "<return>")))))

;;; Evil-cleverparens
(use-package evil-cleverparens
  :ensure t
  :after lsp-mode
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
  (evil-cleverparens-swap-move-by-word-and-symbol t)
  (evil-cleverparens-indent-afterwards nil)
  (evil-cleverparens-use-regular-insert t)
  :hook
  ((emacs-lisp-mode-hook
    lisp-mode-hook
    scheme-mode-hook
    clojure-mode-hook
    clojurec-mode-hook
    clojurescript-mode-hook
    lisp-interaction-mode-hook
    cider-repl-mode-hook
    sly-mrepl-hook) . evil-cleverparens-mode )
  :init
  (advice-add 'evil-cp-set-additional-bindings :around #'do-not-map-M-s-and-M-d))

;;; Aggressive indent
(use-package aggressive-indent
  :disabled t
  :ensure t
  :hook
  (prog-mode-hook . aggressive-indent-mode))

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
                                 motion))
  (evil-escape-excluded-major-modes '(vterm-mode)))

;; (define-key evil-insert-state-map "о" #'kei/maybe-exit)


;; Fancy lambdas
(global-prettify-symbols-mode t)


;;; Which-key
(use-package which-key
  :ensure t
  :custom
  (which-key-show-transient-maps t)
  :config
  (which-key-mode))

;; Keyfreq
(use-package keyfreq
  :defer 0.1
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Free-keys
(use-package free-keys
  :ensure t
  :defer t
  :commands free-keys)

;;; Helpful
(use-package helpful
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("C-h f" . helpful-callable)
        ("C-h v" . helpful-variable)
        ("C-h k" . helpful-key)
        ("C-h F" . helpful-function)
        ("C-h C" . helpful-command)))


;;; Reverse-im
(use-package reverse-im
  :ensure t
  ;; :bind
  ;; ("M-T" . reverse-im-translate-word)
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

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

;;; Fancy
(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 95))

(use-package font-lock+
  :ensure t
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
  :disabled t
  :ensure t
  :hook
  (dired-mode-hook . all-the-icons-dired-mode))

;;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-show-cursor t)
  :bind
  (:map global-map
        ("C-c k t"   . treemacs)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;;; TODO mb add to telega
(use-package emojify
  :ensure t
  :hook (elfeed-search-mode-hook . emojify-mode))

;;; Modeline
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 10)
  ;; (doom-modeline-enable-word-count t)
  ;; (mode-line-compact t) ; emacs 28.1
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

;; Minibuffer
(use-package minibuffer
  :custom
  ;; (completion-cycle-threshold 1)
  (completion-styles '(partial-completion orderless))
  (completion-show-help nil))

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

;; Hook for only checking parens in Lisp modes
(defun lisp-check-parens-after-save-hook ()
  (when (bound-and-true-p lisp-mode)
    ;; (eq major-mode 'lisp-mode)
    (check-parens)))

;;;; Emacs Lisp
(use-package elisp-mode
  :custom
  (eval-expression-print-level t)
  (eval-expression-print-length t)
  ;; (lisp-indent-function 'common-lisp-indent-function)
  :hook
  ;; (after-save-hook . check-parens)
  (after-save-hook . lisp-check-parens-after-save-hook)
  :bind
  (:map emacs-lisp-mode-map
        ("C-c e e" . eval-last-sexp)
        ("C-c e b" . eval-buffer)
        ("C-c e d" . eval-defun)
        ("C-c e p" . eval-print-last-sexp)
        ("C-c e r" . eval-region)
        ("C-c e t" . toggle-debug-on-error)
        ("C-c h d" . describe-function)
        ("C-c h v" . describe-variable)
        ("C-c h x" . describe-command) ; emacs 28.1
        ("C-c h k" . describe-keymap) ; emacs 28.1
        ("C-c h s" . shortdoc-display-group) ; emacs 28.1
        ("C-c h e" . view-echo-area-messages)))

(use-package macrostep
  :ensure t
  :bind
  (:map emacs-lisp-mode-map
        ("C-c m e" . macrostep-expand)))

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
        ("C-c r C" . geiser-connect)
        ("C-c r r" . run-geiser)))

(use-package geiser-mode
  :custom
  (geiser-active-implementations '(racket guile))
  :bind
  (:map geiser-mode-map
        ("C-c e e" . geiser-eval-last-sexp)
        ("C-c e d" . geiser-eval-definition)
        ("C-c e b" . geiser-eval-buffer)
        ("C-c e r" . geiser-eval-region)
        ("C-c e i" . geiser-eval-interrupt)
        ("C-c h d" . geiser-doc-symbol-at-point)
        ("C-c h m" . geiser-repl--doc-module)
        ("C-c m e" . geiser-expand-last-sexp)
        ("C-c m d" . geiser-expand-definition)
        ("C-c m r" . geiser-expand-region)
        ("C-c y"   . geiser-insert-lambda)
        ("C-c r b" . switch-to-geiser)
        ("C-c r q" . geiser-repl-exit)
        ("C-c r c" . geiser-repl-clear-buffer)))

(use-package macrostep-geiser
  :ensure t
  :after (geiser-mode geiser-repl)
  :hook
  ((geiser-mode-hook
    geiser-repl-mode-hook) . macrostep-geiser-setup))

(use-package geiser-guile
  :ensure t)

(use-package geiser-racket
  :ensure t)

;;; https://git.sr.ht/~sokolov/geiser-eros
(use-package geiser-eros
  :ensure t
  :quelpa
  (geiser-eros
   :repo "sokolov/geiser-eros"
   :fetcher sourcehut)
  :hook
  (geiser-mode-hook . geiser-eros-mode))


;;; Counsel
(use-package counsel
  ;; :disabled t
  :ensure t
  :custom
  (ivy-initial-inputs-alist
   '((counsel-minor . "^+")
     (counsel-package . "^+")
     (counsel-org-capture . "^")
     (counsel-M-x . "")
     (counsel-describe-symbol . "^")
     (org-refile . "^")
     (org-agenda-refile . "^")
     (org-capture-refile . "^")
     (Man-completion-table . "^")
     (woman . "^")))
  :bind
  (([remap menu-bar-open] . counsel-tmm)
   ([remap insert-char] . counsel-unicode-char)
   ([remap isearch-forward] . counsel-grep-or-swiper)
   ("C-x b" . counsel-switch-buffer)
   ("C-x 5 b" . counsel-switch-buffer-other-window)
   :map mode-specific-map
   :prefix-map counsel-prefix-map
   :prefix "c"
   ("a" . counsel-apropos)
   ("b" . counsel-bookmark)
   ("B" . counsel-bookmarked-directory)
   ("c w" . counsel-colors-web)
   ("c e" . counsel-colors-emacs)
   ("d" . counsel-dired-jump)
   ("f" . counsel-file-jump)
   ("F" . counsel-faces)
   ("g" . counsel-org-goto)
   ("h" . counsel-command-history)
   ("H" . counsel-minibuffer-history)
   ("i" . counsel-imenu)
   ("j" . counsel-find-symbol)
   ("l" . counsel-locate)
   ("L" . counsel-find-library)
   ("m" . counsel-mark-ring)
   ("o" . counsel-outline)
   ("O" . counsel-find-file-extern)
   ("p" . counsel-package)
   ("r" . counsel-recentf)
   ("s g" . counsel-grep)
   ("s r" . counsel-rg)
   ("s s" . counsel-ag)
   ("t" . counsel-org-tag)
   ("v" . counsel-set-variable)
   ("w" . counsel-wmctrl)
   :map help-map
   ("F" . counsel-describe-face))
  ;; :custom
  ;; (counsel-grep-base-command
  ;;  "rg -i -M 120 --no-heading --line-number --color never %s %s")
  ;; (counsel-search-engines-alist
  ;;  '((google
  ;;     "http://suggestqueries.google.com/complete/search"
  ;;     "https://www.google.com/search?q="
  ;;     counsel--search-request-data-google)
  ;;    (ddg
  ;;     "https://duckduckgo.com/ac/"
  ;;     "https://duckduckgo.com/html/?q="
  ;;     counsel--search-request-data-ddg)))
  :init
  (counsel-mode))

;;; Swiper
(use-package swiper
  :ensure t)

(use-package smex
  :ensure t)

;;; Ivy
(use-package ivy
  :ensure t
  ;; :disabled t
  :custom
  ;; (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-count-format "%d/%d " "Show anzu-like counter")
  (ivy-use-selectable-prompt t "Make the prompt line selectable")
  :custom-face
  (ivy-current-match ((t (:inherit 'hl-line))))
  :bind
  (:map ivy-minibuffer-map
        ("C-j" . ivy-next-line)
        ("C-n" . ivy-next-line)
        ("<down>" . ivy-next-line)
        ("C-k" . ivy-previous-line)
        ("C-p" . ivy-previous-line)
        ("<up>" . ivy-previous-line)
        ("C-l" . ivy-alt-done)
        ;; ("<right>" . ivy-alt-done)
        ("<right>" . forward-char)
        ("<return>" . ivy-done)
        ("C-<return>" . ivy-immediate-done)
        ("C-w" . backward-kill-word)
        ("<tab>" . ivy-partial-or-done)
        :map ivy-switch-buffer-map
        ("C-j" . ivy-next-line)
        ("C-n" . ivy-next-line)
        ("<down>" . ivy-next-line)
        ("C-k" . ivy-previous-line)
        ("C-p" . ivy-previous-line)
        ("<up>" . ivy-previous-line)
        ("C-l" . ivy-alt-done)
        ;; ("<right>" . ivy-alt-done)
        ("<right>" . forward-char)
        ("<return>" . ivy-done)
        ("C-<return>" . ivy-immediate-done)
        ("C-w" . backward-kill-word)
        ("<tab>" . ivy-partial-or-done))
  :config
  ;; (add-to-list 'ivy-re-builders-alist '(t . orderless-ivy-re-builder))
  ;; (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
  (ivy-mode t))

(use-package all-the-icons-ivy
  ;; :defer t
  :ensure t
  :after ivy
  ;; :custom
  ;; (all-the-icons-ivy-buffer-commands '() "Don't use for buffers.")
  :init
  (all-the-icons-ivy-setup))

;; (use-package ivy-rich
;;   :disabled t
;;   :ensure t
;;   :config
;;   (ivy-rich-mode 1))

;; (use-package ivy-prescient
;;   :ensure t
;;   :custom
;;   (ivy-prescient-retain-classic-highlighting t)
;;   :config
;;   (ivy-prescient-mode 1))

;;; Icomplete
(use-package icomplete
  :disabled t
  :after minibuffer
  :custom
  (icomplete-in-buffer t)
  (icomplete-delay-completions-threshold 0)
  (icomplete-max-delay-chars 0)
  (icomplete-compute-delay 0)
  (icomplete-show-common-prefix nil)
  (icomplete-show-matches-on-no-input t)
  (icomplete-prospects-height 10)
  :hook
  (after-init-hook . icomplete-mode)
  :bind
  (:map icomplete-minibuffer-map
        ("C-j" . icomplete-forward-completions)
        ("C-n" . icomplete-forward-completions)
        ("<down>" . icomplete-forward-completions)
        ("C-k" . icomplete-backward-completions)
        ("C-p" . icomplete-backward-completions)
        ("<up>" . icomplete-backward-completions)
        ("C-l" . icomplete-force-complete)
        ("<right>" . icomplete-force-complete)
        ("<return>" . icomplete-force-complete-and-exit)
        ("<tab>" . icomplete-force-complete)
        ("C-w" . kill-whole-line)
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
      (nth 0 completion-in-region--data))))

(use-package icomplete-vertical
  :disabled t
  :ensure t
  :demand t
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
  ;; :disabled t
  :ensure t
  ;; :after icomplete
  :after ivy
  :preface
  (defun orderless-literal-dispatcher (p _ _)
    (when (string-prefix-p "=" p)
      `(orderless-literal . ,(substring p 1))))
  (defun orderless-sli-dispatcher (p _ _)
    (when (string-prefix-p "-" p)
      `(orderless-strict-leading-initialism . ,(substring p 1))))
  :custom
  (orderless-matching-styles '(orderless-flex orderless-regexp))
  (orderless-style-dispatchers '(orderless-literal-dispatcher
                                 orderless-sli-dispatcher)))

;;; PDF
(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-display-size 'fit-height)
  :config
  (pdf-tools-install)
  :hook
  (pdf-view-mode-hook . (lambda () (blink-cursor-mode 0))))

(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :hook
  (pdf-view-mode-hook . pdf-view-restore-mode))

;;;; Highlighting
(use-package hl-line
  :ensure t
  :hook
  (after-init-hook . global-hl-line-mode))

(use-package page-break-lines
  :ensure t
  :hook
  ((help-mode-hook
    prog-mode-hook
    special-mode
    compilation-mode) . page-break-lines-mode))


(use-package rainbow-mode
  :ensure t
  :hook
  ((prog-mode-hook
    help-mode-hook) . rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

;; (use-package prism
;;   :ensure t
;;   :quelpa (prism :fetcher github :repo "alphapapa/prism.el")
;;   :hook
;;   (prog-mode-hook . prism-mode))

;;; Dimming parentheses
;; (use-package paren-face
;;   :ensure t
;;   :hook
;;   (after-init-hook . global-paren-face-mode))

;; (defface paren-face
;;   '((((class color) (background dark))
;;      (:foreground "grey20"))
;;     (((class color) (background light))
;;      (:foreground "grey80")))
;;   "Face used to dim parentheses.")

;; (add-hook 'emacs-lisp-mode-hook
;;  	      (lambda ()
;;  	        (font-lock-add-keywords nil
;;  				                    '(("(\\|)" . 'paren-face)))))

;; Changed evil-goggles to quelpa fetch from my fork with evil-cleverparens support.
;; As it seems that edkovlev is not really maintaining it anymore.

;; (use-package evil-goggles
;;   :ensure t
;;   :custom
;;   (evil-goggles-duration 0.150)
;;   ;; (evil-goggles-enable-delete nil)
;;   :config
;;   (evil-goggles-mode)
;;   (evil-goggles-use-diff-faces))

(use-package evil-goggles
  :quelpa
  (evil-goggles
   :repo "keilisp/evil-goggles"
   :fetcher github)
  :custom
  (evil-goggles-duration 0.150)
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package hl-todo
  :ensure t
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook
  ((prog-mode-hook
    yaml-mode-hook) . hl-todo-mode))

(use-package so-long
  :ensure t
  :hook
  (after-init-hook . global-so-long-mode))

(use-package lisp-extra-font-lock
  :disabled t
  :ensure t
  :hook
  (after-init-hook . lisp-extra-font-lock-global-mode))

;;;; Projectile
(use-package projectile
  ;; :defer 0.2
  :ensure t
  :init (projectile-mode +1)
  :bind
  (:map mode-specific-map ("p" . projectile-command-map))
  :custom
  (projectile-project-search-path (cddr (directory-files "~/code/" t)))
  (projectile-require-project-root nil)
  (projectile-sort-order 'recentf)
  (projectile-project-root-files-functions
   '(projectile-root-local
     projectile-root-top-down
     projectile-root-bottom-up
     projectile-root-top-down-recurring))
  (projectile-completion-system 'ivy))

(use-package counsel-projectile
  :disabled t
  :ensure t
  :after counsel projectile
  :config
  (counsel-projectile-mode))

;;;; Autocompletion
;;; Corfu
(use-package corfu
  :disabled t
  :ensure t
  ;; TAB-and-Go customizations
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)     ; Enable auto completion
  (corfu-auto-delay 0.0)    ; Enable auto completion
  (corfu-quit-at-boundary t)
  (corfu-echo-documentation 0.25)   ; Enable auto completion
  (corfu-preselect-first nil)
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first nil) ;; Disable candidate preselection

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous)
        ([backtab] . corfu-previous))

  :hook
  (after-init-hook . global-corfu-mode))

;;; Company
(use-package company
  ;; :disabled t
  :ensure t
  :custom
  (company-backends '((company-dabbrev-code company-capf company-dabbrev company-files)))
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
  ;; (evil-set-initial-state 'vterm-mode 'emacs)
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
     (lambda ()
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
     (lambda ()
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
        ("C-c k V" . +vterm/here)
        ("C-c k v" . +vterm/toggle)))


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
  (("C-c k l o" . link-hint-open-link)
   ("C-c k l c" . link-hint-copy-link)))

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
  :after google-this
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
  (:map google-this-mode-submap
        ("n" . google-translate-smooth-translate)
        ("T" . kei/google-translate-at-point)))

;;;; FIXME find a better way to define this?
(require 'google-translate)
(defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
(setq google-translate-backend-method 'curl)

;; Google-translate dependency
(use-package popup
  :ensure t)

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

;;;; Snippets
(use-package autoinsert
  :hook
  (find-file . auto-insert))

(use-package yasnippet
  :defer 0.2
  :ensure t
  :custom
  ;; (yas-prompt-functions '(yas-completing-prompt))
  (yas-snippet-dirs '("~/.config/emacs/snippets"))
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
  :hook
  (prog-mode-hook  . yas-minor-mode))

(use-package doom-snippets
  :ensure t
  :quelpa
  (doom-snippets
   :repo "hlissner/doom-snippets"
   :fetcher github
   :files ("*" (:exclude ".*" "README.md")))
  :after yasnippet)

;;; LSP
(use-package lsp-mode
  :ensure t
  :config
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-indentation nil) ;; lsp-format-region bruh
  :hook (((c-mode
           cc-mode
           c++-mode
           sh-mode
           js-mode
           js2-mode
           java-mode
           clojure-mode-hook
           clojurescript-mode-hook
           clojurec-mode-hook
           ) . lsp)
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package compile
  :defer t
  :custom
  (compilation-scroll-output t))

;;;; SQL

(use-package ejc-sql
  :ensure t
  :hook
  (ejc-sql-connected-hook . (lambda ()
                              (ejc-set-fetch-size nil)
                              (ejc-set-max-rows nil)
                              (ejc-set-column-width-limit 80)
                              (ejc-set-use-unicode t))))

;;;; JABA
(use-package lsp-java
  :ensure t)

(use-package log4j-mode
  :ensure t)

;;;; Hideview for toggling code or log blocks
(use-package hideshow
  :bind (("C-c TAB" . hs-toggle-hiding)
         ("C-c C-+" . hs-show-all))
  :hook
  (prog-mode-hook . hs-minor-mode)
  :custom
  (hs-special-modes-alist .
                          (mapcar 'purecopy
                                  '((c-mode "{" "}" "/[*/]" nil nil)
                                    (c++-mode "{" "}" "/[*/]" nil nil)
                                    (java-mode "{" "}" "/[*/]" nil nil)
                                    (js-mode "{" "}" "/[*/]" nil)
                                    (json-mode "{" "}" "/[*/]" nil)
                                    (javascript-mode  "{" "}" "/[*/]" nil)))))

;;;; Clojure
(use-package flymake-kondor
  :disabled
  :ensure t
  :hook
  ((clojure-mode-hook
    clojurec-mode-hook
    clojurescript-mode-hook) . flymake-kondor-setup))

(use-package eldoc
  :ensure t
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package cider
  :ensure t)

(use-package cider-mode
  :init
  (advice-add 'cider-insert-last-sexp-in-repl :around #'evil-collection-cider-last-sexp)
  (advice-add 'cider-pprint-eval-last-sexp :around #'evil-collection-cider-last-sexp)
  (advice-add 'cider-pprint-eval-last-sexp-to-comment :around #'evil-collection-cider-last-sexp)
  :custom
  (cider-font-lock-dynamically nil)
  (cider-font-lock-reader-conditionals nil)
  (cider-overlays-use-font-lock t)
  (cider-save-file-on-load t)
  (cider-font-lock-dynamically '(macro core function var))
  :config
  (defun cider-insert-last-sexp-in-repl-and-eval ()
    (interactive)
    (cider-insert-last-sexp-in-repl t))

  ;; FIXME insert-last-sexp is faster than cider-repl-set-ns???
  (defun cider-repl-set-ns-insert-last-sexp-and-eval ()
    (interactive)
    (call-interactively #'cider-repl-set-ns)
    (sit-for 0.05) ;; HACK
    (cider-insert-last-sexp-in-repl-and-eval))
  :bind
  (:map cider-mode-map
        ("C-c x"   . cider-interrupt)
        ("C-c r c" . cider-find-and-clear-repl-output)
        ("C-c r q" . cider-quit)
        ("C-c e u" . cider-undef)
        ("C-c e e" . cider-eval-last-sexp)
        ("C-c e E" . cider-repl-set-ns-insert-last-sexp-and-eval)
        ("C-c e i" . cider-insert-last-sexp-in-repl)
        ("C-c e I" . cider-insert-last-sexp-in-repl-and-eval)
        ("C-c e b" . cider-eval-buffer)
        ("C-c e d" . cider-eval-defun-at-point)
        ("C-c e r" . cider-eval-region)
        ("C-c e t" . cider-eval-defun-up-to-point)
        ("C-c e f" . cider-load-all-files)
        ("C-c e n" . cider-eval-ns-form)
        ("C-c e N" . cider-ns-refresh)
        ("C-c E e" . cider-pprint-eval-last-sexp)
        ("C-c E E" . cider-pprint-eval-last-sexp-to-comment)
        ("C-c E d" . cider-pprint-eval-defun-at-point)
        ("C-c E D" . cider-pprint-eval-defun-to-comment)
        ("C-c n n" . cider-repl-set-ns)
        ("C-c n b" . cider-browse-ns)
        ("C-c n f" . cider-find-ns)
        ("C-c n e" . cider-eval-ns-form)
        ("C-c h d" . cider-doc)
        ("C-c h j" . cider-javadoc)
        ("C-c h c" . cider-clojuredocs)
        ("C-c h a" . cider-apropos)
        ("C-c h A" . cider-apropos-documentation)
        ("C-c h n" . cider-browse-ns)
        ("C-c h s" . cider-browse-spec)
        ("C-c h S" . cider-browse-spec-all)
        ("C-c t n" . cider-test-run-ns-tests)
        ("C-c t t" . cider-test-run-test)
        ("C-c t p" . cider-test-run-project-tests)
        ("C-c t f" . cider-test-rerun-failed-tests)
        ("C-c m m" . cider-macroexpand-1)
        ("C-c m M" . cider-macroexpand-all)))

(use-package cider-eldoc
  :after cider-mode eldoc
  :custom
  (cider-eldoc-display-context-dependent-info t))

(use-package cider-common
  :after cider-mode
  :custom
  (cider-prompt-for-symbol nil)
  (cider-special-mode-truncate-lines nil))

(use-package cider-repl
  :after cider-mode
  :bind
  (:map cider-repl-mode-map
        ("C-c q" . cider-quit)
        ("C-c c" . cider-repl-clear-buffer)
        ;; ("C-k" . cider-repl-previous-input )
        ;; ("C-j" . cider-repl-next-input)
        ;; ("C-p" . cider-repl-previous-input)
        ;; ("C-n" . cider-repl-next-input)
        )
  :custom
  ;; (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-pop-to-buffer-on-connect nil)
  ;; (cider-repl-display-in-current-window t)
  ;; (cider-repl-buffer-size-limit 600)
  :init
  (advice-add 'cider-repl--insert-banner :override #'ignore)
  :hook
  (cider-repl-mode . (lambda () (setq truncate-lines t))))

(use-package cider-completion
  :after cider-mode
  :custom
  (cider-completion-use-context nil)
  (cider-completion-annotations-include-ns 'always))

(use-package clojure-mode
  :ensure t
  :custom
  (clojure-indent-style 'align-arguments)
  ;; (clojure-indent-style 'always-align)
  (clojure-toplevel-inside-comment-form t)
  :hook
  (clojure-mode-hook . (lambda () (setq-local comment-column 0)))
  ;; (before-save-hook . #'cider-format-buffer)
  ;; (before-save-hook . #'lsp-format-buffer)
  :custom-face
  (clojure-keyword-face ((t (:slant italic :inherit font-lock-constant-face))))
  :bind
  (:map clojure-mode-map
        ("C-c r b" . cider-connect-clj)
        ("C-c r f" . cider-connect-cljs)
        ("C-c r j" . cider-jack-in)
        :map evil-normal-state-map
        ("# j" . clojure-toggle-ignore)
        ("# f" . clojure-toggle-ignore-surrounding-form)
        ("# d" . clojure-toggle-ignore-defun)))

(use-package nrepl-client
  :defer t
  :hook
  (nrepl-connected-hook . (lambda () (switch-to-buffer-other-frame (cider-current-repl))))
  :custom
  (nrepl-hide-special-buffers t))

;; TODO maybe use there functions from clojure-mode (C-c C-r)
(use-package clj-refactor
  :disabled t
  :ensure t
  :hook
  ((clojure-mode-hook
    clojurec-mode-hook
    clojurescript-mode-hook) . clj-refactor-mode)
  :bind
  (:map clojure-mode-map
        ("C-c r t t" . cljr-thread)
        ("C-c r t l" . cljr-thread-last-all)
        ("C-c r t f" . cljr-thread-first-all)
        ("C-c r t u" . cljr-unwind)
        ("C-c r t a" . cljr-unwind-all)
        ("C-c r c i" . cljr-cycle-if)
        ("C-c r c p" . cljr-cycle-privacy)
        ("C-c r c t" . cljr-cycle-thread)
        ("C-c r e f" . cljr-extract-function)
        ("C-c r e d" . cljr-extract-def)
        ("C-c r l i" . cljr-introduce-let)
        ("C-c r l e" . cljr-expand-let)
        ("C-c r l m" . cljr-move-to-let)
        ("C-c r l r" . cljr-remove-let)
        ("C-c r a" . cljr-add-missing-libspec)
        ("C-c r d" . cljr-destructure-keys)
        ("C-c r [" . clojure-convert-collection-to-vector)
        ("C-c r {" . clojure-convert-collection-to-map)
        ("C-c r (" . clojure-convert-collection-to-list)
        ("C-c r '" . clojure-convert-collection-to-quoted-list)
        ("C-c r #" . clojure-convert-collection-to-set)))

;; https://ag91.github.io/blog/2022/06/09/make-adding-a-clojure-require-more-interactive-with-cider-and-cljr/
;; (defun my/make-cljr-add-use-snippet-interactive ()
;;   (setq-local cljr--add-use-snippet "[${1:$$(yas-choose-value (ignore-errors (cider-sync-request:ns-list)))} :refer ${2:[$3]}]"))

;; (add-hook 'cider-mode-hook 'my/make-cljr-add-use-snippet-interactive)
;;;;

;; (use-package anakondo
;;   :ensure t
;;   :hook
;;   (clojure-mode-hook . anakondo-minor-mode)
;;   (clojurescript-mode-hook . anakondo-minor-mode)
;;   (clojurec-mode-hook . anakondo-minor-mode))

;; (use-package flycheck-clj-kondo
;;   :ensure t)

;;; R
(use-package ess
  :ensure t)

;; Docker
(use-package docker-tramp
  :ensure t)

;;;;;;;;;;;;;;;;;;;;
;;; QBP specific ;;;
;;;;;;;;;;;;;;;;;;;;

(ejc-create-connection
 "qbp_bdm_demo_dev_v2"
 :classpath (cider-jar-find-or-fetch "mysql" "mysql-connector-java" "5.1.44")
 :subprotocol "mysql"
 :subname "//172.17.0.1:3406/bdm_demo_dev_v2?autoReconnect=true&useSSL=false"
 :user "root"
 :password "root")

;;; Debugger
;; (use-package dap-mode
;;   :defer t
;;   :ensure t
;;   ;; :init
;;   ;; (dap-gdb-lldb-setup)
;;   :custom
;;   (dap-auto-configure-mode t                           "Automatically configure dap.")
;;   (dap-auto-configure-features
;;    '(sessions locals breakpoints expressions tooltip)  "Remove the button panel in the top.")
;;   :config
;;   (require 'dap-gdb-lldb)
;;   :bind
;;   (:map dap-mode-map
;;         ("C-c d s" . dap-debug)
;;         ("C-c d p" . dap-debug-last)
;;         ("C-c d n" . dap-debug-last)
;;         ("C-c d t" . dap-breakpoint-toggle)
;;         ("C-c d c" . dap-breakpoint-condition)
;;         ("C-c d h" . dap-breakpoint-hit-condition)
;;         ("C-c d H" . dap-hydra)
;;         ("C-c d l" . dap-breakpoint-log-message)
;;         ("C-c d D" . dap-breakpoint-delete)
;;         ("C-c d P" . dap-ui-breakpoints)))


;;;; CC
(use-package ccls
  :ensure t)

(use-package gdb
  :hook
  (gdb-mode-hook . gdb-many-windows)
  :hook
  (c-mode-hook . (lambda ()
                   (define-key c-mode-base-map (kbd "C-c d g") 'gdb)
                   (define-key c-mode-base-map (kbd "C-c d W") 'gdb-many-windows)
                   (define-key c-mode-base-map (kbd "C-c d b") 'gud-break)
                   (define-key c-mode-base-map (kbd "C-c d d") 'gud-remove)
                   (define-key c-mode-base-map (kbd "C-c d r") 'gud-remove)
                   (define-key c-mode-base-map (kbd "C-c d R") 'gud-refresh)
                   (define-key c-mode-base-map (kbd "C-c d p") 'gud-print)
                   (define-key c-mode-base-map (kbd "C-c d n") 'gud-next)
                   (define-key c-mode-base-map (kbd "C-c d w") 'gud-watch)
                   (define-key c-mode-base-map (kbd "C-c d c") 'gud-cont)
                   (define-key c-mode-base-map (kbd "C-c d s") 'gud-step)
                   (define-key c-mode-base-map (kbd "C-c d x") 'gud-finish))))


;;;; Rust
(use-package rustic
  :ensure t
  :bind
  (:map rustic-mode-map
        ("C-c c c" . rustic-compile)
        ("C-c c r" . rustic-recompile)
        ("C-c c s" . rustic-compile-send-input)
        ("C-c f b" . rustic-format-buffer)
        ("C-c f f" . rustic-format-file)
        ("C-c f w" . rustic-cargo-fmt)
        ;; TODO install cargo-edit
        ("C-c e a" . rustic-cargo-add)
        ("C-c e r" . rustic-cargo-rm)
        ("C-c e u" . rustic-cargo-upgrade)
        ("C-c e o" . rustic-cargo-outdated)
        ("C-c t t" . rustic-cargo-test)
        ("C-c t r" . rustic-cargo-test-rerun)
        ("C-c t c" . rustic-cargo-current-test)
        ;;; FIXME gdb
        ("C-c d g" . gdb)
        ("C-c d W" . gdb-many-windows)
        ("C-c d b" . gud-break)
        ("C-c d d" . gud-remove)
        ("C-c d r" . gud-remove)
        ("C-c d R" . gud-refresh)
        ("C-c d p" . gud-print)
        ("C-c d n" . gud-next)
        ("C-c d w" . gud-watch)
        ("C-c d c" . gud-cont)
        ("C-c d s" . gud-step)
        ("C-c d x" . gud-finish)))

;;; Lua (just for awesomewm)
(use-package lua-mode
  :ensure t)

;;; Fennel
(use-package fennel-mode
  :ensure t
  :bind
  (:map fennel-mode-map
        ("C-c r c" . fennel-repl)
        ("C-c f f" . fennel-format)
        ("C-c e e" . lisp-eval-last-sexp)
        ("C-c e d" . lisp-eval-defun)
        ("C-c e e" . lisp-eval-region)
        ("C-c e b" . fennel-reload)
        ("C-c m m" . fennel-macroexpand)
        ("C-c h d" . fennel-show-documentation)
        ("C-c h v" . fennel-show-variable-documentation)
        ("C-c h a" . fennel-show-arglist)
        ("C-c h c" . fennel-show-compilation)))

;;; Common Lisp
(use-package sly
  :ensure t
  :custom
  (inferior-lisp-program "/etc/profiles/per-user/kei/bin/sbcl")
  :hook
  (sly-connected-hook . (lambda () (switch-to-buffer-other-frame (sly-mrepl))))
  ;; (sly-mode-hook . (lambda ()
  ;;                    (unless (sly-connected-p)
  ;;                      (save-excursion (sly)))))
  :bind
  (:map sly-mode-map
        ;; sly-mrepl-mode-map
        ("C-c r b" . sly)
        ("C-c r c" . sly-connect)
        ("C-c r r" . sly-mrepl)
        ("C-c r n" . sly-mrepl-new)
        ("C-c r s" . sly-mrepl-sync)
        ("C-c r C" . sly-mrepl-clear-repl)
        ("C-c r o" . sly-mrepl-clear-recent-output)
        ("C-c r x" . sly-interrup)
        ;; sly-doc-map
        ("C-c h l" . sly-documentation-lookup)
        ("C-c h s" . sly-describe-symbol)
        ("C-c h f" . sly-describe-function)
        ("C-c h a" . sly-apropos)
        ("C-c h A" . sly-apropos-all)
        ("C-c h h" . sly-hyperspec-lookup)
        ("C-c h F" . hyperspec-lookup-format)
        ("C-c h R" . hyperspec-lookup-reader-macro)
        ("C-c e u" . sly-undefine-function)
        ("C-c e i" . sly-interactive-eval)
        ("C-c e e" . sly-eval-last-expression)
        ("C-c e d" . sly-eval-defun)
        ("C-c e r" . sly-eval-region)
        ("C-c e l" . sly-load-file)
        ("C-c e p" . sly-pprint-eval-last-expresion)
        ("C-c c d" . sly-compile-defun)
        ("C-c c i" . sly-interactive-eval)
        ("C-c c f" . sly-compile-file)
        ("C-c c l" . sly-compile-and-load-file)
        ("C-c c r" . sly-compile-region)
        ("C-c c m" . sly-compiler-macroexpand-1)
        ("C-c c M" . sly-compiler-macroexpand)
        ("C-c c D" . sly-disassemble-symbol)
        ("C-c s"   . sly-scratch)
        ("C-c n d" . sly-stickers-dwim)
        ("C-c n r" . sly-stickers-replay)
        ("C-c n t" . sly-stickers-toggle-break-on-stickers)
        ("C-c n f" . sly-stickers-fetch)
        ("C-c n n" . sly-stickers-next-sticker)
        ("C-c n p" . sly-stickers-prev-sticker)
        ("C-c m m" . sly-macroexpand-1)
        ("C-c m M" . sly-macroexpand-all)
        ("C-c m s" . sly-format-string-expand)
        ("C-c I"   . sly-inspect)
        ("C-c t t" . sly-trace-dialog-toggle-trace)
        ("C-c t T" . sly-trace-dialog)
        ;; sly-who-map
        ("C-c w u" . sly-edit-uses)
        ("C-c w c" . sly-who-calls)
        ("C-c w C" . sly-calls-who)
        ("C-c w r" . sly-who-references)
        ("C-c w b" . sly-who-binds)
        ("C-c w s" . sly-who-sets)
        ("C-c w m" . sly-who-macroexpands)
        ("C-c w S" . sly-who-specializes)))

(use-package sly-quicklisp
  :ensure t)

(use-package sly-named-readtables
  :ensure t)

(use-package sly-macrostep
  :ensure t
  :bind
  (:map sly-prefix-map
        ("C-c m e" . macrostep-expand)))

(use-package sly-asdf
  :ensure t)

;;;; Nix
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :bind
  (:map nix-mode-map
        ("C-c b" . nix-build)
        ("C-c r r" . nix-repl-show)
        ("C-c r s" . nix-repl-shell)
        ("C-c U" . nix-unpack)))

(use-package nix-update
  :ensure t
  :bind
  (:map nix-mode-map
        ("C-c u" . nix-update-fetch)))

(use-package ivy-nixos-options
  :disabled t
  :ensure t
  :bind
  (:map nix-mode-map
        ("C-c d" . ivy-nixos-options)))

;;; Direnv (lorri)
(use-package direnv
  :ensure t
  :config
  (direnv-mode))

;;;; Shell
(use-package sh-mode
  :hook
  (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(use-package company-shell
  :ensure t
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env)))

;;;; JSON
(use-package json-mode
  :ensure t)

(use-package json-snatcher
  :ensure t
  :bind
  (:map js-mode-map
        ("C-c p" . jsons-print-path))
  :hook '(js-mode js2-mode))

;;;; JS & TS
;; TODO mb config fully
;; + [[https://github.com/defunkt/coffee-mode][coffee-mode]]
;; + [[https://github.com/mooz/js2-mode][js2-mode]]
;; + [[https://github.com/felipeochoa/rjsx-mode][rjsx-mode]]
;; + [[https://github.com/emacs-typescript/typescript.el][typescript-mode]]
;; + [[https://github.com/magnars/js2-refactor.el][js2-refactor]]
;; + [[https://github.com/mojochao/npm-mode][npm-mode]]
;; + [[https://github.com/abicky/nodejs-repl.el][nodejs-repl]]
;; + [[https://github.com/skeeto/skewer-mode][skewer-mode]]
;; + [[https://github.com/ananthakumaran/tide][tide]]
;; + [[https://github.com/NicolasPetton/xref-js2][xref-js2]]*

(use-package coffee-mode
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package rjsx-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package skewer-mode
  :ensure t)

(use-package tide
  :ensure t)

;;;; Markdown
(use-package markdown-mode
  :ensure t)

;;;; Latex
(use-package auctex
  :defer t
  :ensure t)

(use-package adaptive-wrap
  :ensure t
  :hook
  (LaTeX-mode-hook . adaptive-wrap-prefix-mode))

(use-package evil-tex
  :ensure t)

(use-package latex-preview-pane
  :ensure t
  :hook
  (LaTeX-mode-hook . latex-preview-pane-mode)
  :bind
  (:map LaTeX-mode-map
        ("C-c u u" . latex-preview-pane-update)
        ("C-c u s" . latex-preview-pane-mode)))

(use-package company-auctex
  :ensure t
  :init
  (company-auctex-init))

(use-package company-math
  :ensure t
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode))

;;; Magit

;; (use-package keychain-environment
;;   :ensure t
;;   :init
;;   (keychain-refresh-environment))

(use-package ssh-agency
  :ensure t)

(use-package magit
  :ensure t
  :custom
  (magit-log-margin '(t age-abbreviated magit-log-margin-width t 7))
  (magit-diff-refine-hunk t)
  (magit-completing-read-function 'ivy-completing-read "Force Ivy usage.")
  :bind
  ;; (:map magit-mode-map
  ;;       ("<tab>" . magit-section-toggle))
  ("C-c p m" . magit-project-status)
  ("C-c v a" . magit-stage-file)       ; the closest analog to git add
  ("C-c v b" . magit-blame)
  ("C-c v B" . magit-branch)
  ("C-c v c" . magit-checkout)
  ("C-c v C" . magit-commit)
  ("C-c v d" . magit-diff)
  ("C-c v D" . magit-discard)
  ("C-c v f" . magit-fetch)
  ("C-c v g" . vc-git-grep)
  ("C-c v G" . magit-gitignore)
  ("C-c v i" . magit-init)
  ("C-c v l" . magit-log)
  ("C-c v m" . magit)
  ("C-c v M" . magit-merge)
  ("C-c v n" . magit-notes-edit)
  ("C-c v p" . magit-pull-branch)
  ("C-c v P" . magit-push-current)
  ("C-c v r" . magit-reset)
  ("C-c v R" . magit-rebase)
  ("C-c v s" . magit-status)
  ("C-c v S" . magit-stash)
  ("C-c v t" . magit-tag)
  ("C-c v T" . magit-tag-delete)
  ("C-c v u" . magit-unstage)
  ("C-c v U" . magit-update-index))

(use-package evil-collection-magit
  :disabled t
  :after magit
  :custom
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-y-for-yank t))

(use-package forge
  :defer t
  :after magit
  :ensure t)

(use-package smerge-mode
  :defer t)

(use-package browse-at-remote
  :ensure t
  :bind
  (("C-c k l r" . browse-at-remote)))

;;; Org
(use-package calendar
  :defer t
  :custom (calendar-week-start-day 1))

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode-hook . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(use-package org-ql
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
                  :files (:defaults (:exclude "helm-org-ql.el")))
  :after org)

(use-package org
  :hook
  (org-mode-hook . variable-pitch-mode)
  (org-mode-hook . org-num-mode)
  (org-mode-hook . org-indent-mode)
  :bind (("C-c o c" . org-capture)
         :map org-mode-map
         ("C-c '"   . org-edit-src-code)
         ("C-c o q" . org-set-tags-command)
         ("C-c o i" . org-insert-structure-template)
         ("C-c o t" . org-todo)
         ("C-c o p" . org-set-property)
         ("C-c o P" . org-priority)
         ("C-c o r" . org-refile)
         ("C-c o s" . org-schedule)
         ("C-c o d" . org-deadline)
         :map org-src-mode-map
         ("C-c '"   . org-edit-src-exit))
  :custom
  (org-default-notes-file "~/org/todo.org")
  (org-hidden-keywords '(title author date startup))
  (org-hide-emphasis-markers t)
  (org-todo-keywords '((sequence "TODO(t!/!)" "NEXT(n!/!)"
                                 "STARTED(s!)" "|" "DONE(d!/!)")
                       (type "PROJECT(p!/!)" "|" "DONE_PROJECT(D!/!)")
                       (sequence "WAIT(w@/!)" "SOMEDAY(S!)" "|"
                                 "CNCL(c@/!)")))
  (org-tag-alist '(("study" . ?s)
                   ("nixos"  . ?n)
                   ("video" . ?v)
                   ("prog"   . ?p)
                   ("listen" . ?l)
                   ("read"  . ?r)
                   ("emacs"  . ?e)
                   ("gtd"   . ?t)
                   ("work"  . ?w)
                   ("idea"  . ?i)
                   ("en")
                   ("life" . ?d)
                   ("guix"  . ?x)))
  (org-startup-folded 'overview)
  (org-ellipsis "...")
  (org-pretty-entities t)
  (org-direcotry "~/org")
  ;; (org-adapt-indentation nil)
  (org-hide-leading-stars t)
  (org-image-actual-width nil)
  ;; (org-export-latex-packages-alist (quote (("" "cmap" t) ("ukrainian,russian,english" «babel» t))))
  ;; (org-export-default-language "ru")
  (org-startup-folded t)
  (org-refile-targets '((nil :maxlevel . 2)
                        (org-agenda-files :maxlevel . 4)
                        ("archive.org" :maxlevel . 2)))
  :config
  (add-to-list 'org-latex-default-packages-alist '("" "cmap" t))
  (add-to-list 'org-latex-default-packages-alist '("ukrainian,russian,english" "babel" t))
  (add-to-list 'org-latex-default-packages-alist '("T2A" "fontenc" t))

  (defun org-get-level-face (n)
    "Get the right face for match N in font-lock matching of headlines."
    (let* ((org-l0 (- (match-end 2) (match-beginning 1) 1))
           (org-l (if org-odd-levels-only (1+ (/ org-l0 2)) org-l0))
           (org-f (if org-cycle-level-faces
                      (nth (% (1- org-l) org-n-level-faces) org-level-faces)
                    (nth (1- (min org-l org-n-level-faces)) org-level-faces))))
      (cond
       ((eq n 1) (if org-hide-leading-stars 'org-hide org-f))
       ((eq n 2) 'org-hide)
       (t (unless org-level-color-stars-only org-f)))))

  (defun my/show-next-without-effort ()
    (interactive)
    (org-ql-search (org-agenda-files)
      '(and (todo "NEXT")
            (not (property "Effort")))))

  (defun my/generate-agenda-weekly-review ()
    "Generate the agenda for the weekly review"
    (interactive)
    (let ((span-days 24)
          (offset-past-days 10))
      (message "Generating agenda for %s days starting %s days ago"
               span-days offset-past-days)
      (org-agenda-list nil (- (time-to-days (date-to-time
                                             (current-time-string)))
                              offset-past-days)
                       span-days)
      (org-agenda-log-mode)
      (goto-char (point-min))))

  (defun my/search-random-someday ()
    "Search all agenda files for SOMEDAY and jump to a random item"
    (interactive)
    (let* ((todos '("SOMEDAY"))
           (searches (mapcar (lambda (x) (format "+TODO=\"%s\"" x)) todos))
           (joint-search (string-join searches "|") )
           (org-agenda-custom-commands '(("g" tags joint-search))))
      (org-agenda nil "g")
      (let ((nlines (count-lines (point-min) (point-max))))
        (goto-line (random nlines)))))

  (setq org-capture-templates `())

  (setq org-capture-templates
        `(
          ;; ("i" "Inbox" entry
          ;;  (file+olp "~/org/gtd/inbox.org")
          ;;  "* TODO %?\n%U\n\n  %i"
          ;;  :kill-buffer t)
          ;; ("t" "Todo with link" entry
          ;;  (file "~/org/gtd/inbox.org")
          ;;  "* TODO %?\n%U\n\n  %i\n  %a"
          ;;  :kill-buffer t)
          ("t" "Todo" entry
           (file+olp "~/org/todo.org")
           "* TODO %t %? :gtd:\n")
          ("w" "Work Todo" entry
           (file+olp "~/org/todo.org")
           "* TODO %t %? :work:\n")
          ("b" "Bookmark" entry
           (file+olp "~/org/bookmarks.org")
           "* [%?[][]] \n")
          ("s" "Study" entry
           (file+olp "~/org/learning.org")
           "\n* [%?[][]] :study: \n")
          ("r" "Read" entry
           (file+olp "~/org/learning.org")
           "\n* [%?[][]] :read: \n")
          ("v" "Video" entry
           (file+olp "~/org/learning.org")
           "\n* [%?[][]] :video: \n")
          ("l" "Listen" entry
           (file+olp "~/org/learning.org")
           "\n* [%?[][]] :listen: \n")
          ("p" "Prog" entry
           (file+olp "~/org/learning.org")
           "\n* [%?[][]] :prog: \n")
          ("i" "Idea" entry
           (file+olp+datetree "~/org/ideas.org")
           "* %T %? :idea:\n")))

  ;; Save org-files after refile
  (advice-add 'org-refile :after
              (lambda (&rest _)
                (org-save-all-org-buffers))))

(use-package org-habit
  ;; :after org
  :custom
  (org-habit-show-habits-only-for-today t))

;; (use-package org-latex
;;   :config
;;   (add-to-list 'org-latex-default-packages-alist '("ukrainian,russian,english" "babel" t)))

(use-package org-src
  :custom (org-src-window-setup 'current-window))

(use-package org-agenda
  :bind (("C-c o a" . org-agenda-list)
         ("C-c o A" . org-agenda)
         ("C-c o r" . org-agenda-refile))
  :config
  (setq org-agenda-files '("~/org/todo.org"
                           "~/org/learning.org"
                           "~/org/university.org"
                           "~/org/birthdays.org"
                           "~/org/habits.org"
                           "~/org/bookmarks.org"
                           "~/org/life.org"
                           "~/org/gtd/org-gtd-tasks.org"))

  ;; (setq org-agenda-files '("~/org/"))

  (setq org-agenda-custom-commands
        '(("d" "Daily Action List"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-sorting-strategy
                      '((agenda time-up category-up tag-up))))
                    (org-deadline-warning-days 7))))

          ("j" . "Job Todo")
          ("jj" tags "+work+TODO=\"TODO\"")
          ("jn" "Job" tags "+work+TODO=\"NEXT\"|+work+TODO=\"STARTED\"")

          ;; ("g" "Inbox" tags "+gtd"
          ;;  ((org-agenda-overriding-header "Things to get done")
          ;;   (org-agenda-files org-agenda-files)))

          ;; ("p" "Practice" tags "+prog"
          ;;  ((org-agenda-overriding-header "Things to learn on practice")
          ;;   (org-agenda-files org-agenda-files)))

          ;; ("s" "Projects" tags "+study"
          ;;  ((org-agenda-overriding-header "Things to study")
          ;;   (org-agenda-files org-agenda-files)))

          ;; ("w" tags "+write"
          ;;  ((org-agenda-overriding-header "Things that include writing")
          ;;   (org-agenda-files org-agenda-files)))

          ;; ("d" "Life" tags "+life"
          ;;  ((org-agenda-overriding-header "Things to todo in longer distance")
          ;;   (org-agenda-files org-agenda-files)))

          ("l" "Listen"
           tags "+listen"
           ((org-agenda-overriding-header "Things to listen")
            (org-agenda-files org-agenda-files)))

          ("v" "Watch"
           tags "+video"
           ((org-agenda-overriding-header "Things to watch")
            (org-agenda-files org-agenda-files)))

          ;; ("V" tags "+video+prog"
          ;;  ((org-agenda-overriding-header "Things to watch and practice")
          ;;   (org-agenda-files org-agenda-files)))

          ("r" "Read"
           tags "+read"
           ((org-agenda-overriding-header "Things to read")
            (org-agenda-files org-agenda-files)))

          ;; ("e" tags "+emacs"
          ;;  ((org-agenda-overriding-header "Things to hack on emacs")
          ;;   (org-agenda-files org-agenda-files)))

          ;; ("x" tags "+guix"
          ;;  ((org-agenda-overriding-header "Things to hack on guix")
          ;;   (org-agenda-files org-agenda-files)))

          ;; ("n" tags "+nixos"
          ;;  ((org-agenda-overriding-header "Things to hack on nixos")
          ;;   (org-agenda-files org-agenda-files)))

          ("u" "Univerisy"
           tags "+university"
           ((org-agenda-overriding-header "Things for university")
            (org-agenda-files org-agenda-files)))

          ("P" "Stuck Projects"
           ((org-ql-block '(and (todo "PROJECT")
                                (or (not (descendants))
                                    (and (not (descendants
                                               (or (todo "STARTED") (todo "NEXT"))))
                                         (descendants
                                          (and (or (todo "TODO") (todo "WAITING"))
                                               (not (deadline))
                                               (not (scheduled)))))
                                    (not (descendants (or (todo "NEXT") (todo "TODO")
                                                          (todo "WAITING")
                                                          (todo "STARTED")))))))))))

  :custom
  (org-agenda-skip-scheduled-if-done . nil)
  (org-agenda-skip-deadline-if-done . nil)
  (org-agenda-span 10))

(use-package org-super-agenda
  :ensure t)

;; (let ((org-super-agenda-groups
;;        '(;; Each group has an implicit boolean OR operator between its selectors.
;;          (:name "Today"  ; Optionally specify section name
;;                 :time-grid t  ; Items that appear on the time grid
;;                 :todo "TODAY")  ; Items that have this TODO keyword
;;          (:name "Important"
;;                 ;; Single arguments given alone
;;                 :tag "bills"
;;                 :priority "A")
;;          ;; Set order of multiple groups at once
;;          (:order-multi (2 (:name "Shopping in town"
;;                                  ;; Boolean AND group matches items that match all subgroups
;;                                  :and (:tag "shopping" :tag "@town"))
;;                           (:name "Food-related"
;;                                  ;; Multiple args given in list with implicit OR
;;                                  :tag ("food" "dinner"))
;;                           (:name "Personal"
;;                                  :habit t
;;                                  :tag "personal")
;;                           (:name "Space-related (non-moon-or-planet-related)"
;;                                  ;; Regexps match case-insensitively on the entire entry
;;                                  :and (:regexp ("space" "NASA")
;;                                                ;; Boolean NOT also has implicit OR between selectors
;;                                                :not (:regexp "moon" :tag "planet")))))
;;          ;; Groups supply their own section names when none are given
;;          (:todo "WAITING" :order 8)  ; Set order of this section
;;          (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
;;                 ;; Show this group at the end of the agenda (since it has the
;;                 ;; highest number). If you specified this group last, items
;;                 ;; with these todo keywords that e.g. have priority A would be
;;                 ;; displayed in that group instead, because items are grouped
;;                 ;; out in the order the groups are listed.
;;                 :order 9)
;;          (:priority<= "B"
;;                       ;; Show this section after "Today" and "Important", because
;;                       ;; their order is unspecified, defaulting to 0. Sections
;;                       ;; are displayed lowest-number-first.
;;                       :order 1)
;;          ;; After the last group, the agenda will display items that didn't
;;          ;; match any of these groups, with the default order position of 99
;;          )))
;;   (org-agenda nil "a"))

(use-package org-roam
  :ensure t
  :bind (("C-c o C" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n u" . org-roam-buffer-refresh)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n r" . org-roam-ref-add)
         ;; :map org-mode-map
         ;; ("C-c n i" . org-roam-insert)
         ;; ("C-c n I" . org-roam-insert-immediate)
         )
  :config
  (setq org-roam-directory
        (file-truename "~/org/roam/"))
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (org-roam-db-autosync-mode)

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (setq org-roam-mode-sections
        (list
         #'org-roam-backlinks-section
         #'org-roam-reflinks-section
         #'org-roam-unlinked-references-section
         )
        )
  :custom
  ;; (org-roam-mode-section-functions
  ;;  (list
  ;;   #'org-roam-backlinks-section
  ;;   #'org-roam-reflinks-section
  ;;   ;; #'org-roam-unlinked-references-section
  ;;   )
  ;;  )

  (org-roam-node-display-template
   (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "main/${slug}.org"
                         "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     ("r" "reference" plain "%?"
      :if-new
      (file+head "reference/${title}.org" "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     ("a" "article" plain "%?"
      :if-new
      (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
      :immediate-finish t
      :unnarrowed t)
     ("b" "book" plain "%?"
      :if-new
      (file+head "books/${title}.org" "#+title: ${title}\n#+filetags: :book:\n")
      :immediate-finish t
      :unnarrowed t)
     ("p" "project" plain "%?"
      :if-new
      (file+head "projects/${title}.org" "#+title: ${title}\n#+filetags: :project:\n")
      :immediate-finish t
      :unnarrowed t)
     ("t" "talk" plain "%?"
      :if-new
      (file+head "talks/${title}.org" "#+title: ${title}\n#+filetags: :talk:\n")
      :immediate-finish t
      :unnarrowed t)
     ("j" "job" plain "%?"
      :if-new
      (file+head "job/${title}.org" "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t))))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package deft
  :ensure t
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory)
  :config
  ;; thx https://github.com/jrblevin/deft/issues/75#issuecomment-905031872
  (defun my/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
          (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
        (deft-base-filename file))))
  
  (advice-add 'deft-parse-title :override #'my/deft-parse-title)
  
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
                "\\)")))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-edna
  :ensure t)

(use-package org-gtd
  :ensure t
  :after org
  :quelpa (org-gtd :fetcher github :repo "trevoke/org-gtd.el"
                   :branch "main" :upgrade t)
  :demand t
  :custom
  (org-gtd-directory "~/org/gtd/")
  (org-edna-use-inheritance t)
  :config
  (org-edna-mode)
  (advice-add 'org-gtd--refile :after
              (lambda (&rest _)
                (org-save-all-org-buffers)))
  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-show-stuck-projects)
   ("C-c d A" . org-gtd-archive-completed-items)
   :map org-gtd-process-map
   ("C-c d c" . org-gtd-choose)))

(use-package hledger-mode
  :ensure t
  :custom
  (hledger-jfile "~/dox/finance/2022.journal")
  (hledger-currency-string "₴")
  :mode ("\\.journal\\'" "\\.hledger\\'" "\\.dat\\'" )
  :preface
  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry))

  :bind (("C-c j" . hledger-run-command)
         :map hledger-mode-map
         ("C-c e" . hledger-jentry)
         ("C-c *" . hledger-toggle-star)
         ("M-p" . hledger/prev-entry)
         ("M-n" . hledger/next-entry)))

(use-package flycheck-hledger
  :ensure t
  :after (flycheck ledger-mode))


;; Dashboard
;; ALWAYS IN THE END!
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
  (dashboard-startup-banner (or (when (file-exists-p "~/pix/doom/M-x_butterfly_smaller.png")
                                  "~/pix/doom/M-x_butterfly_smaller.png")
                                'official))
  (dashboard-center-content t)
  (dashboard-set-footer nil)
  (dashboard-items '(
                     ;; (recents  . 5)
                     ;; (bookmarks . 5)
                     ;; (registers . 5)
                     ;; (projects . 5)
                     (agenda . 5)))
  :config
  (defun dashboard-insert-agenda (&rest _)
    "Insert a copy of org-agenda buffer."
    (insert (save-window-excursion
              (org-agenda-list)
              (prog1 (buffer-string)
                (kill-buffer)))))
  ;; (setq dashboard-week-agenda t)
  ;; (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (dashboard-setup-startup-hook)
  (dashboard-insert-agenda)
  (dashboard-refresh-buffer))
