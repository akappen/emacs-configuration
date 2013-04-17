;;; package configuration
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;; global configuration

;; interface tweaks
(setq inhibit-splash-screen t)    ; no welcome screen
(tool-bar-mode 0)                 ; no icon bar
(scroll-bar-mode 0)               ; no scrollbar
(global-linum-mode t)             ; line numbers
(require 'linum-off)              ;   where appropriate
(column-number-mode t)            ; modeline column numbers
(winner-mode t)                   ; window layout history
(defalias 'yes-or-no-p 'y-or-n-p) ; short prompts
(setq make-backup-files nil)      ; no ~ backup files
(load-theme 'twilight t)          ; tango-dark is nice too
(add-hook 'before-save-hook       ; clean whitespace on save
	  'delete-trailing-whitespace)
(show-paren-mode t)               ; highlight matching braces
(electric-pair-mode t)            ; pair quotes and braces
(electric-indent-mode t)          ; auto indent where appropriate
(delete-selection-mode t)         ; input replaces selected region

;; put buffer name or file path in frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; allow scroll-down/up-command to move point to buffer end/beginning
(setq scroll-error-top-bottom 'true)

;; remap other-window to M-o
(global-set-key (kbd "M-o") 'other-window)

;; C-h and C-M-h as delete and delete word
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; grep-find ignores
(grep-compute-defaults)
(add-to-list 'grep-find-ignored-directories "log")
(add-to-list 'grep-find-ignored-directories "tmp")

;; show path hints for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; package specific configuration

;; magit
(setq magit-status-buffer-switch-function 'switch-to-buffer) ; status opens full-frame
(global-set-key "\C-xg" 'magit-status)

;; browse-kill-ring
(browse-kill-ring-default-keybindings) ; M-y opens browser

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-modes 'haml-mode)
(ac-config-default)

;; coffee-mode
(require 'coffee-mode)
(setq coffee-tab-width 2)

;; ruby-tools
(require 'ruby-tools) ; needed to hook ruby-mode

;; flymake-ruby
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; rinari
(require 'rinari)
(global-rinari-mode)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; org-mode
(setq org-directory "~/org")
(setq org-agenda-files (directory-files org-directory 1 "\.org$"))
(setq org-agenda-clockreport-parameter-plist (quote (:fileskip0 t)))
(global-set-key "\C-ca" 'org-agenda)
