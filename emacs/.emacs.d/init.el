;;;; use-package ;;;;

(require 'package) ;; Emacs builtin

;; set package.el repositories
(setq package-archives
'(
   ("org" . "https://orgmode.org/elpa/")
   ("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
))

;; initialize built-in package management
(package-initialize)

;; update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

;; a list of pkgs to programmatically install
;; ensure installed via package.el
(setq my-package-list '(use-package))

;; programmatically install/ensure installed
;; pkgs in your personal list
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;; UI ;;;;

(scroll-bar-mode -1)
(set-frame-font "PragmataPro Mono Liga 16" nil t)

;;;; rust-mode ;;;;
(use-package rust-mode
  :ensure t
  :config
  (require 'eglot)
  (add-hook 'rust-mode-hook 'eglot-ensure))

;;;; c-c++ mode ;;;;
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;;;; company ;;;;
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))
