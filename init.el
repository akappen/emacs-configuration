;;; package configuration
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;; global configuration

(setq inhibit-startup-screen t)   ; no welcome screen
(menu-bar-mode t)                 ; enable menus
(global-linum-mode t)             ; line numbers
(column-number-mode t)            ; modeline column numbers
(winner-mode t)                   ; window layout history
(delete-selection-mode t)         ; overwrite/delete selection
(global-auto-revert-mode t)       ; reload external file changes
(electric-indent-mode t)          ; auto indent where appropriate
(load-theme 'twilight t)          ; tango-dark is nice too

;; nicer scrolling defaults
(setq scroll-margin 2)
(setq scroll-error-top-bottom t)
(setq scroll-preserve-screen-position t)

;; disable file interlocking
(setq create-lockfiles nil)

;; short prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; clean whitespace on save
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)

;; set font to Source Code Pro if available
;; https://github.com/adobe/source-code-pro
(if (eq window-system 'x)
    (if (x-list-fonts "SourceCodePro")
        (set-frame-font "SourceCodePro-11" t t)))

;; put buffer name or file path in frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; window navigation
;; (global-set-key (kbd "M-o") 'next-multiframe-window)
(global-set-key (kbd "M-o") 'switch-window) ; testing switch-window
;; (global-set-key (kbd "M-O") 'previous-multiframe-window)

;; C-h and M-h as delete and delete word
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; camelcase editing in programming modes
(add-hook 'prog-mode-hook 'syntax-subword-mode)

;; spell check comments in programming modes
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; grep-find ignores
(grep-compute-defaults)
(add-to-list 'grep-find-ignored-directories "log")
(add-to-list 'grep-find-ignored-directories "tmp")

;; C-c g runs vc-git-grep
(global-set-key (kbd "C-c g") 'vc-git-grep)

;; toggle dedicated window
(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

;; display-buffer-alist for grep buffers
(add-to-list 'display-buffer-alist
             `(,(rx bos "*grep*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))
;; display-buffer-alist for alchemist test buffers
(add-to-list 'display-buffer-alist
             `(,(rx bos "*alchemist test report*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))

(defun ak-quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))
(global-set-key (kbd "C-c q") #'ak-quit-bottom-side-windows)

;; find recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 400)
(setq recentf-max-menu-items 80)
(defun ido-recentf-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key (kbd "C-x f") 'ido-recentf-find-file)


;;; package specific configuration

;; powerline
;; separators: alternate arrow arrow-fade bar box brace butt chamfer
;; contour curve rounded roundstub slant wave zigzag
;; themes: default center center-evil vim nano
(require 'powerline)
(setq powerline-default-separator 'brace)
(powerline-default-theme)
;; adjust active/inactive colors
(set-face-attribute 'mode-line nil :box nil :foreground "white")
(set-face-attribute 'mode-line-inactive nil :box nil :weight 'unspecified :background "gray75" :foreground "gray50")

;; flx-ido
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(setq ido-use-virtual-buffers t)

;; ido-vertical-mode
(require 'ido-vertical-mode)
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; magit
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
(global-set-key "\C-xg" 'magit-status)

;; browse-kill-ring
(browse-kill-ring-default-keybindings) ; M-y opens browser

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(setq sp-highlight-pair-overlay nil)
(require 'smartparens-ruby)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-SPC") 'mc/mark-all-dwim)

;; linum-off
(require 'linum-off) ; disables linum-mode where appropriate

;; auto-complete
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (ac-flyspell-workaround)
;; (add-to-list 'ac-modes 'haml-mode)
;; (add-to-list 'ac-modes 'coffee-mode)
;; (define-key ac-completing-map [down] nil)
;; (define-key ac-completing-map [up] nil)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 4)


;; js-mode
(setq js-indent-level 2)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)

;; scss-mode
(setq css-indent-offset 2)

;; coffee-mode
(require 'coffee-mode)
(setq coffee-tab-width 2)

;; ruby-mode
(setq ruby-deep-indent-paren nil)

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

;; elixir
(require 'elixir-mode)
(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
         :when '(("SPC" "RET"))
         :actions '(insert navigate))
  (sp-local-pair "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(sp-ruby-def-post-handler)
         :actions '(insert navigate)))

;; alchemist
(require 'alchemist)

;; org-mode
(setq org-directory "~/org")
(setq org-agenda-files (directory-files org-directory 1 "\.org$"))
(setq org-agenda-clockreport-parameter-plist (quote (:fileskip0 t)))
(setq org-agenda-start-with-clockreport-mode t)
(setq org-agenda-start-with-log-mode t)
(global-set-key "\C-ca" 'org-agenda)
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
