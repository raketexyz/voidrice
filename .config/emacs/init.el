(load-theme 'wombat)

(setq gc-cons-threshold 12800000)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(use-package vterm
    :ensure t)
(use-package rust-mode)
(use-package lua-mode)
(use-package magit)
(use-package gnuplot)
(use-package ledger-mode)
(use-package lsp-mode
    :init (setq lsp-keymap-prefix "C-c l")
    :hook ((gdscript-mode . lsp)
	   (rust-mode . lsp)
           (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)
(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))

(menu-bar-mode -1)
(tool-bar-mode -1)

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(column-number-mode)
(setq-default c-basic-offset 4)
(setq-default lisp-indent-offset 4)
(setq-default lua-indent-level 4)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-agenda-files '("/home/rakete/Documents/org/dates.org"
			 "/home/rakete/Documents/org/log.org"
			 "/home/rakete/Documents/org/life.org"
			 "/home/rakete/Documents/org/todo.org"
			 "/home/rakete/Documents/org/projects.org"))

(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "mail.rakete.xyz"
      smtpmail-smtp-user "rakete@rakete.xyz"
      smtpmail-stream-type 'ssl
      smtpmail-smtp-service 465
      smtpmail-servers-requiring-authorization "rakete.xyz")

(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "mailhost.cms.hu-berlin.de"
      smtpmail-smtp-user "davisala"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587
      smtpmail-servers-requiring-authorization "hu-berlin.de")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "librewolf")

(setq-default fill-column 80)
(add-hook 'auto-fill-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'auto-fill-mode)
