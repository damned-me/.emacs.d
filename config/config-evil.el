;; config-evil.el

(defun rune/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook (evil-mode . rune/evil-hook)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;(define-key evil-normal-state-map (kbd "C-i") 'scroll-down-command)
;(define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)


;; Imagine the following scenario.  One wants to paste some previously copied
;; (from application other than Emacs) text to the system's clipboard in place
;; of some contiguous block of text in a buffer.  Hence, one switches to
;; `evil-visual-state' and selects the corresponding block of text to be
;; replaced.  However, one either pastes some (previously killed) text from
;; `kill-ring' or (if `kill-ring' is empty) receives the error: "Kill ring is
;; empty"; see `evil-visual-paste' and `current-kill' respectively.  The
;; reason why `current-kill' does not return the desired text from the
;; system's clipboard is because `evil-visual-update-x-selection' is being run
;; by `evil-visual-pre-command' before `evil-visual-paste'.  That is
;; `x-select-text' is being run (by `evil-visual-update-x-selection') before
;; `evil-visual-paste'.  As a result, `x-select-text' copies the selected
;; block of text to the system's clipboard as long as
;; `x-select-enable-clipboard' is non-nil (and in this scenario we assume that
;; it is).  According to the documentation of `interprogram-paste-function',
;; it should not return the text from the system's clipboard if it was last
;; provided by Emacs (e.g. with `x-select-text').  Thus, one ends up with the
;; problem described above.  To solve it, simply make
;; `evil-visual-update-x-selection' do nothing:
(fset 'evil-visual-update-x-selection 'ignore)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; I don't know why but on my arch installation `evil-collection-init`
;; doesen't seem to work inside the use-package definition so I execute
;; it twice, once after the package definition.
(evil-collection-init)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))
