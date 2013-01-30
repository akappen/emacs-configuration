;;; required package configuration

;; additional package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; install or update packages
(defvar my-packages
  (list 'magit
	'browse-kill-ring
	'linum-off
	'coffee-mode
	'haml-mode
	'rspec-mode
	'ruby-tools
	'flymake-ruby
	'rinari
	'markdown-mode
	'twilight-theme))
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
