;; --------------------------------------------------------
;; Visual custom settings
;; --------------------------------------------------------
(custom-set-variables
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(frame-background-mode (quote dark))
 '(line-number-mode t)
 '(show-trailing-whitespace t)
 '(solarized-termcolors 256)
 '(tool-bar-mode nil))

(setq initial-scratch-message "")  ;; Empty scratch-buffer message
(setq inhibit-startup-message t)   ;; No welcome screen

;; Change startup screen size
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 220))

;; --------------------------------------------------------
;; solarized theme: sellout/emacs-color-theme-solarized
;; Found visual issues with powerline in bbatsov/solarized-emacs
;; --------------------------------------------------------
(use-package color-theme
  :ensure t
  :pin melpa
  :catch (lambda (keyword err)
           (message (error-message-string err))))

(setq color-theme-libraries nil) ;; Avoid loading nonexisting themes

(use-package color-theme-solarized
  :load-path "lisp/emacs-color-theme-solarized"
  ;; :ensure t
  ;; :pin melpa
  :config
  (customize-set-variable 'frame-background-mode 'dark)
  (load-theme 'solarized t))    ;; Load without confirm

;; (use-package solarized-theme
;;   :ensure t
;;   :pin melpa
;;   :config
;;   (load-theme 'solarized-dark t))

;; --------------------------------------------------------
;; Powerline - nicer status line
;; Issues? Look at:
;;         http://irrationalrose.com/2015/05/29/workaround-for-srgb-colour-issue-for-powerline-on-os-x.html
;;         or check variable in powerline
;;
;; Updated with
;; https://github.com/milkypostman/powerline/issues/101
;; --------------------------------------------------------
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

(provide 'init-visuals)
