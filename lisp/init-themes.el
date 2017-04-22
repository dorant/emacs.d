(load-theme 'solarized t)  ; Load without confirm

;; Powerline - nicer status line
;; Issues? Look at:
;;         http://irrationalrose.com/2015/05/29/workaround-for-srgb-colour-issue-for-powerline-on-os-x.html
(add-to-list 'load-path (expand-file-name "lisp/powerline" user-emacs-directory))
(require 'powerline)
(powerline-default-theme)
(setq powerline-default-separator 'slant)

;; Solarized dark theme
(custom-set-variables
 '(frame-background-mode (quote dark)))
(enable-theme 'solarized)

(provide 'init-themes)
