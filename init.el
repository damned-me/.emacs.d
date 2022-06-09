;; init.el

(load "server")
(unless (server-running-p) (server-start))

;; set up package.el to work with MELPA
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")
    ("elpa" . "https://elpa.gnu.org/packages/")
    ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; fetch the list of packages available and initialize it
(unless package-archive-contents
  (package-refresh-contents))

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; load all configs
(let ((bindings-directory (concat user-emacs-directory "bindings/"))
    (config-directory (concat user-emacs-directory "config/")))
  (cl-loop for file in (append (reverse (directory-files-recursively config-directory "\\.el$"))
                               (reverse (directory-files-recursively bindings-directory "\\.el$")))
           do (condition-case ex
                  (load (file-name-sans-extension file)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(org-roam vterm org-fancy-priorities org-super-agenda helm-org org-helm eterm-256color evil-nerd-commenter org-wiki org-tempo doom-themes doom-modeline all-the-icons evil-collection evil helpful ivy-rich dap-mode lsp-treemacs lsp-ivy lsp-ui lsp-mode markdown-toc forge magit counsel-projectile projectile hydra which-key rainbow-delimiters visual-fill-column use-package rustic org-bullets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
