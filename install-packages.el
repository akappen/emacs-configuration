;;; required package configuration

;; additional package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; install or update packages
(defvar my-packages
  (list 'better-defaults
        'powerline
        'browse-kill-ring
        'syntax-subword
        'smartparens
        'multiple-cursors
        'linum-off
        'company
        'twilight-theme
        'solarized-theme
        'flx-ido
        'ido-vertical-mode
        'switch-window
        'flycheck
        'magit
        'web-mode
        'scss-mode
        'coffee-mode
        'haml-mode
        'markdown-mode
        ;; ruby
        'rspec-mode
        'ruby-tools
        'rinari
	;; elixir
	'elixir-mode
	'alchemist))
(defun my-packages-install ()
  "Install my packages."
  (interactive)
  (dolist (package my-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

;; execute
(package-refresh-contents)
(my-packages-install)
