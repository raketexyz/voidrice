;;; init.el -*- lexical-binding: t -*-

;;; Garbage collection
(setq gc-cons-threshold 12800000)

;;; Packages
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
(use-package pdf-tools
  :config (pdf-tools-install))
(use-package auctex
  :ensure t)

;;; General UI options
(load-theme 'wombat)

(menu-bar-mode -1)
(tool-bar-mode -1)

;;; Text editing
(setq backward-delete-char-untabify-method nil)
(setq-default display-line-numbers-type 'relative)
(setq-default fill-column 80)

(add-hook 'auto-fill-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'column-number-mode)

;;; TeX editing
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(setq-default LaTeX-indent-level 4)
(setq-default LaTeX-item-indent 0)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;;; Source code editing
(setq whitespace-style '(face tabs tab-mark trailing))
(setq-default indent-tabs-mode nil)
(global-whitespace-mode)
(setq-default inferior-lisp-program "sbcl")

(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-agenda-files '("/home/rakete/Documents/org/dates.org"
                         "/home/rakete/Documents/org/log.org"
                         "/home/rakete/Documents/org/life.org"
                         "/home/rakete/Documents/org/todo.org"
                         "/home/rakete/Documents/org/projects.org"))

;;; Mail
(setq send-mail-function 'smtpmail-send-it)

(defun smtp-server-rakete ()
  "Set the `smtpmail-' variables for `mail.rakete.xyz'."
  (interactive)
  (setq smtpmail-smtp-server "mail.rakete.xyz"
        smtpmail-smtp-user "rakete@rakete.xyz"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
        smtpmail-servers-requiring-authorization "rakete.xyz"))

(defun smtp-server-hu ()
  "Set the `smtpmail-' variables for `mailhost.cms.hu-berlin.de'."
  (interactive)
  (setq smtpmail-smtp-server "mailhost.cms.hu-berlin.de"
        smtpmail-smtp-user "davisala"
        smtpmail-stream-type 'starttls
        smtpmail-smtp-service 587
        smtpmail-servers-requiring-authorization "hu-berlin.de"))

;;; Browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "librewolf")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-current-line ((t (:inherit line-number :weight bold)))))
