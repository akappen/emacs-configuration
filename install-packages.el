;;; required package configuration

;; additional package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; install or update packages
(defvar my-packages
  (list 'better-defaults
        'browse-kill-ring
        'smartparens
        'multiple-cursors
        'linum-off
        'auto-complete
        'twilight-theme
        'flx-ido
        'ido-vertical-mode
        'switch-window
        'magit
        ;; coffeescript
        'coffee-mode
        ;; haml
        'haml-mode
        ;; ruby
        'rspec-mode
        'ruby-tools
        'flymake-ruby
        'rinari
        ;; clojure
        'cider
        ;; markdown
        'markdown-mode))
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
