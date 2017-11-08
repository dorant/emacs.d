;;-------------------------------------------------------------
;;; Theme hooks
(defvar my/theme-hooks nil
  "((theme-id . function) ...)")

(defun my/add-theme-hook (theme-id hook-func)
  (add-to-list 'my/theme-hooks (cons theme-id hook-func)))

(use-package solarized
  :ensure solarized-theme
  :defer t
  :init
  (defun my/solarized-theme-hook ()
    (set-face-attribute 'font-lock-constant-face nil :weight 'normal)
    (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
    (set-face-attribute 'which-key-key-face nil :foreground
                        (face-attribute 'error :foreground)))
  (my/add-theme-hook 'solarized-dark  #'my/solarized-theme-hook)
  (my/add-theme-hook 'solarized-light #'my/solarized-theme-hook))
  ;; :config
  ;; (setq solarized-use-variable-pitch nil
  ;;       solarized-use-less-bold t
  ;;       solarized-use-more-italic nil
  ;;       solarized-distinct-doc-face t
  ;;       solarized-high-contrast-mode-line t
  ;;       ;; I find different font sizes irritating.
  ;;       solarized-height-minus-1 1.0
  ;;       solarized-height-plus-1 1.0
  ;;       solarized-height-plus-2 1.0
  ;;       solarized-height-plus-3 1.0
  ;;       solarized-height-plus-4 1.0))

;; Set theme
;;(load-theme 'solarized-dark  t)
;;-------------------------------------------------------------


(add-to-list 'custom-theme-load-path (expand-file-name "themes/emacs-color-theme-solarized" user-emacs-directory))

(load-theme 'solarized t)  ; Load without confirm

(custom-set-variables
 '(frame-background-mode (quote dark)) ;; Solarized dark theme
 '(solarized-termcolors 256)
 )
(enable-theme 'solarized)


;; Powerline - nicer status line
;; Issues? Look at:
;;         http://irrationalrose.com/2015/05/29/workaround-for-srgb-colour-issue-for-powerline-on-os-x.html
;;         or check variable in powerline
;;
;; Updated with
;; https://github.com/milkypostman/powerline/issues/101
(use-package powerline
  :load-path "lisp/powerline")

(setq powerline-default-separator 'slant)
(setq powerline-display-buffer-size nil)
(setq powerline-display-mule-info nil)
;;(powerline-default-theme)

(defun powerline-default-linenum-theme ()
  "Setup the default mode-line with line number lhs."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (powerline-raw "%4l" nil 'l)
                                     (powerline-raw ":" nil 'l)
                                     (powerline-raw "%3c" nil 'r)
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size nil 'l))
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info nil 'l))
                                     (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) face1 'l))
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face2 face1)))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-default-linenum-theme)

(provide 'init-themes)
