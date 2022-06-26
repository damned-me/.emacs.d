(setq inhibit-startup-message t)
(setq epa-pinentry-mode 'loopback)
(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1)   ; disable toolbar
(tooltip-mode -1)    ; disable tooltips
(set-fringe-mode 10) ; give some breathing room

(menu-bar-mode -1)

(set-face-attribute 'default nil :font "DejaVuSansMono" :height 125)
(set-face-attribute 'fixed-pitch nil :font "DejaVuSansMono" :height 125)
(set-face-attribute 'variable-pitch nil :font "DejaVuSansMono" :height 125 :weight 'regular)


(column-number-mode)
;(display-line-numbers-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative) 

(dolist (mode '(term-mode-hook
;		org-mode-hook
		eww-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
