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
(set-default-font "SourceCodePro-11")
(load-theme 'twilight t)          ; tango-dark is nice too
(add-hook 'before-save-hook       ; clean whitespace on save
	  'delete-trailing-whitespace)
(show-paren-mode t)               ; highlight matching braces
(electric-pair-mode t)            ; pair quotes and braces
(electric-indent-mode t)          ; auto indent where appropriate

;; put buffer name or file path in frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; camelcase editing in programming modes
(add-hook 'prog-mode-hook 'subword-mode)

;; allow scroll-down/up-command to move point to buffer end/beginning
(setq scroll-error-top-bottom t)

;; scroll-down/up-command returns to starting row and column
(setq scroll-preserve-screen-position t)

;; window navigation
(global-set-key (kbd "M-o") 'next-multiframe-window)
(global-set-key (kbd "M-O") 'previous-multiframe-window)

;; C-h and M-h as delete and delete word
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; spell check comments in programming modes
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; grep-find ignores
(grep-compute-defaults)
(add-to-list 'grep-find-ignored-directories "log")
(add-to-list 'grep-find-ignored-directories "tmp")

;; show path hints for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; open and indent new line from anywhere
(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key [(shift return)] 'smart-open-line)
(global-set-key (kbd "C-M-j") 'smart-open-line)

;; find recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)


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
(ac-config-default)
(ac-flyspell-workaround)
(add-to-list 'ac-modes 'haml-mode)
(add-to-list 'ac-modes 'coffee-mode)

;; coffee-mode
(require 'coffee-mode)
(setq coffee-tab-width 2)

;; ruby-mode
(setq ruby-deep-indent-paren nil) ; force 2 space indent of multi-line hash literals

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

;; flx-ido
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil)

;; ido-vertical-mode
(require 'ido-vertical-mode)
(ido-vertical-mode)

;; org-mode
(setq org-directory "~/org")
(setq org-agenda-files (directory-files org-directory 1 "\.org$"))
(setq org-agenda-clockreport-parameter-plist (quote (:fileskip0 t)))
(setq org-agenda-start-with-clockreport-mode t)
(setq org-agenda-start-with-log-mode t)
(global-set-key "\C-ca" 'org-agenda)
