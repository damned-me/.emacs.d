;; bindings.el

(defun reload-init-file ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c C-l") 'reload-init-file)    ; Reload .emacs file
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c C-k") 'magit-kill-this-buffer)
(global-set-key (kbd "C-ò") 'hydra-text-scale/text-scale-decrease)
(global-set-key (kbd "C-c b") 'eaf-open-browser-with-history)
(global-set-key (kbd "C-c o") 'eaf-open-this-buffer)
; TODO upgrade to general (use-package general)
