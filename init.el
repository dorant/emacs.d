;*********************************************************
;
; Description
;  Setup file for emacs
;
; See
; https://github.com/tdd11235813/emacs_config/blob/master/lisp/init/init_cpp.el#L81
;*********************************************************
(message "Loading ~/.emacs.d/init.el ...")
;*********************************************************

; Own init- packages
(add-to-list 'load-path (expand-file-name "~/bin/rtags/share/emacs/site-lisp/rtags/"))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; --------------------------------------------------------
;; Initiate helpers and modes
;; --------------------------------------------------------
(require 'init-package-handler)         ;; Package handler
(require 'init-evil-matchit-mode)       ;; %-key handler, jump between matchin tags
(require 'init-solarized-theme)         ;; Theme

;;(require 'init-python-mode)           ;; Python mode
;(require 'init-flycheck)
;(require 'init-company)
;(require 'init-cmake-ide)             ;;

;(use-package irony)
;(add-hook 'c++-mode-hook 'irony-mode)
;(add-hook 'c-mode-hook 'irony-mode)



(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)



(use-package cmake-mode) ;; CMake color

;(use-package irony)

(load "c_mode_setup")
(load "plantuml")

;; GIT
;;(use-package magit)

; --------------------------------------------------------
; Syntax highlighting support for "Modern C++" - until C++17
; --------------------------------------------------------
(use-package modern-cpp-font-lock
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  (add-hook 'c-mode-hook   #'modern-c++-font-lock-mode)
  )

; --------------------------------------------------------
; Show colors instead of controlchars in shell and compilation
; --------------------------------------------------------
(use-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
    "Colorize the compilation buffer."
    (read-only-mode)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  )


;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

; --------------------------------------------------------
; Avoid killing Emacs by mistake
; --------------------------------------------------------
(defun dont-kill-emacs()
  "Disable C-x C-c binding execute kill-emacs."
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

(global-set-key "\C-x\C-c"               'dont-kill-emacs)

; --------------------------------------------------------
; What to run when make
; --------------------------------------------------------
(defvar make-program "gmake -f ~/Makefile"
   "Default program to run at `make'.")

; --------------------------------------------------------
; TAGS
; --------------------------------------------------------
(setq tags-file-name "~/tmp/TAGS")

; --------------------------------------------------------
; GIT
; --------------------------------------------------------
;(require 'git)
;(require 'git-blame)
;(autoload 'git-blame-mode "git-blame"
;  "Minor mode for incremental blame for Git." t)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(use-package server
  :defer t
  :config
  (progn
    (unless (server-running-p)
      (server-start))))


;;----------------------------------------------------------------------------
;; Move to emacs-utils
;;----------------------------------------------------------------------------
(defun match-bracket (arg)
  "Go to the matching bracket if on bracket, otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        ((looking-at "\\s\{") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\}") (forward-char 1) (backward-list 1))
        ((looking-at "\\s\[") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\]") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))
  )
)

(defun list-buffers-same-window ()
  "List all buffers in split-screen and move to buffer-list"
  (interactive)
  (list-buffers)
  (other-window 1)
)

;;----------------------------------------------------------------------------
;; Key setup
;;----------------------------------------------------------------------------

; MacOSX bindings
(if (string-equal "darwin" system-type)
    (progn
      (message "Mac OSX key-bindigs ...")
      (setq mac-option-modifier nil
            mac-command-modifier 'meta
            x-select-enable-clipboard t
            mac-option-key-is-meta nil
            mac-command-key-is-meta t
            )
      ))

(setq cc-other-file-alist
      '(("\\.c$"   (".h"))
        ("\\.cc$"  (".h"))
        ("\\.cpp$" (".h"))
        ("\\.h$"   (".c" ".cc" ".cpp"))))
(setq cc-search-directories
      '("."
        "../src/*" "../include/*"
        "../test-src/*" "../test-include/*"))


(global-set-key [(f1)]            'other-window)             ; Jump between windows
(global-set-key [(shift f1)]      'goto-line)                ; Goto line in file
(global-set-key [(f2)]            'indent-for-comment)       ; Comment line after (C/C++)
(global-set-key [(f3)]            'comment-region)           ; Comment a market region
(global-set-key [(shift f3)]      'uncomment-region)         ; Uncomment a market region
(global-set-key [(f4)]            'next-error)               ; Next error in a compiler result
(global-set-key [(f8)]            'list-buffers-same-window) ; List all buffers in a window
(global-set-key [(f10)]           'ff-get-other-file)        ; Get corresponding .cc or .hh file
(global-set-key [(shift f10)]     'revert-buffer)            ; Refresh the buffer contents from file

(global-set-key "%"               'match-bracket)        ; Jump between scopes, simple (or just writing '%')
(global-set-key [(control ?5)]    'evilmi-jump-items)    ; Jump between scopes, fuller

(global-set-key "\C-cc"           'compile)
(global-set-key "\C-c\C-c"        'make)

(global-set-key [(home)]          'beginning-of-buffer)
(global-set-key [(end)]           'end-of-buffer)


;;----------------------------------------------------------------------------
;; Custom settings
;;----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-always-kill t)
 '(compilation-scroll-output (quote first-error))
 '(indent-tabs-mode nil)
 '(line-number-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil))

(setq initial-scratch-message "")  ;; Empty scratch-buffer message
(setq inhibit-startup-message t)   ;; No welcome screen

;; Change startup screen size
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 220))

;*********************************************************
(message "Loading ~/.emacs.d/init.el done")
;*********************************************************
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
