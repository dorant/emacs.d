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

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; --------------------------------------------------------
;; Initiate helpers and modes
;; --------------------------------------------------------
(require 'init-package-handler)         ;; Package handler
(require 'init-evil-matchit-mode)       ;; Jump between matching tags

(require 'init-themes)         ;; Themes and powerline

(require 'move-border)         ;; Functions for moving buffers relative its position

(require 'init-plantuml)

(defun white-background ()
  (interactive)
  (setq buffer-face-mode-face `(:background "white"))
  (buffer-face-mode 1))

;;(require 'init-python-mode)           ;; Python mode
;;(require 'init-flycheck)
(require 'init-company)
;(require 'init-cmake-ide)             ;;

(require 'whitespace)
;;(setq whitespace-style '(tabs tab-mark)) ;turns on white space mode only for tabs
(global-whitespace-mode 0)


;;(use-package irony)
;;(add-hook 'c++-mode-hook 'irony-mode)
;;(add-hook 'c-mode-hook 'irony-mode)

(require 'rtags)
;; (require 'company-rtags)

;; (setq rtags-completions-enabled t)
;;  (eval-after-load 'company
;;    '(add-to-list
;;     'company-backends 'company-rtags))
;; (setq rtags-autostart-diagnostics t)
;; (rtags-enable-standard-keybindings)

;;;; Speedbar
;; (add-to-list 'load-path (expand-file-name "lisp/sr-speedbar" user-emacs-directory))
;; (require 'sr-speedbar)
;; (setq speedbar-use-images nil)
;; (sr-speedbar-open)
;; (with-current-buffer sr-speedbar-buffer-name
;;   (setq window-size-fixed 'width))

(use-package cmake-mode) ;; CMake color

;(use-package irony)

(load "c_mode_setup")
(load "plantuml")

;; GIT
(use-package magit)


(require 'ffap)  ; Find File At Point
(ffap-bindings)  ; Dxefault key bindings


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

;; Avoid break in shell
(setenv "PAGER" "cat")

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

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(use-package server
  :ensure t
  :init
  (server-mode 1)
  :config
  (unless (server-running-p)
    (server-start)))

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
  (while (not (string-equal "*Buffer List*" (buffer-name)))
    (other-window 1))
)

(defun dos-to-unix ()
  "Convert a DOS buffer to unix format."
  (interactive)
  (beginning-of-buffer)
  (replace-string "\r\n" "\n")
)

(defun copy-word ()
  "Marks and copies word at or before point.
   to kill ring, and save in the X clipboard."
  (interactive)
  (let (beg end word)
    (save-excursion

      ;; Find end of word
      (while (looking-at "[^ \n()\"\t:'`,]")
        (forward-char 1))
      (setq end (point))
      (forward-char -1)

      ;; Find start of word
      (while (looking-at "[^ \n()\"\t:'`,]")
        (forward-char -1))
      (forward-char 1)
      (setq beg (point))

      (message "copy-word=%s" (buffer-substring beg end))
      (clipboard-kill-ring-save beg end))
    )
)

(defun file-note-jump ( )
  "This function jumps to a file location found at point in a
   `file-note-buffer'  previously stored with `file-note'.

   The syntax of the location in a text line can have three shapes namely :
   1. ... /path/file (L%d) ... or
   2. ... /path/file :%d: ...  or
   3. ... ./path/file :%d: ... or
   4. file :%d: ...            or         (format produced by fgrep -n)
   5. ... tag    (L%d) ...
   where %d is a short for an integer

   If a file is given according to syntax 1-4 , `find-file-at-point' and `goto-line' %d is performed.
   If a tag is given  according to syntax 4, `find-tag' and `goto-line' %d is performed.
   The command can typically be used when jumping from a trace/debug printout directly into the source code."
  (interactive)

  ;; Current line at (point)
  (setq cur-line (buffer-substring (progn (beginning-of-line) (point)) (progn (end-of-line) (point) )))

  (cond
  ;; Does the line contain a complete filename and a linenumber within (L%d) or :%d:
  ;; Example:
  ;; /vobs/bom/bcmcmi/bin/bcmcmiMain.cc (L58)    ...
  ;; ./bom/bcmcmi/bin/bcmcmiMain.cc:47:...
  ;; ... /vobs/bom/bcmcmi/bin/bcmcmiMain.cc:47:...
  ;; bcmcmiMain.cc:47:...                              ( format produced by fgrep -n)

   ((or (string-match "^[^/]*\\(/[^ ]+\\)[ ]+([ ]*L\\([0-9]+\\)[ ]*)" cur-line)      ; /vobs/bom/bcmcmi/bin/bcmcmiMain.cc (L58)   ...
        (string-match "^[^.]*\\([.]+/[^ :]+\\)[ ]*:[ ]*\\([0-9]+\\)[ ]*:" cur-line)   ; ./bom/bcmcmi/bin/bcmcmiMain.cc:47:...
        (string-match "^[^/]*\\(/[^ :]+\\)[ ]*:[ ]*\\([0-9]+\\)[ ]*:" cur-line)    ; ... /vobs/bom/bcmcmi/bin/bcmcmiMain.cc:47:...
        (string-match "^[ ]*\\([^ :]+\\)[ ]*:[ ]*\\([0-9]+\\)[ ]*:" cur-line))     ;
    (let*  (
            ( filename (substring cur-line (match-beginning 1) (match-end 1)))
            ( line     (substring cur-line (match-beginning 2) (match-end 2)))
            )

      (message (format "find-file(%s) (L%s)" filename  line))
      (find-file-at-point filename)
      (goto-line (string-to-int line))
      ))

   ;; Does the line contain a tag or file name (with or without extension) and a linenumber within (L ) e.g ...
   ;; ... bcmcmiMain    (L58)    ... or
   ;; ... bcmcmiMain.cc (L58)    ...

   ;;   ((string-match "\\([^ ./,]+\\)\\([.][^ ]*\\)?[ ]+([ ]*L\\([0-9]+\\)[ ]*)" cur-line)
   ;;   ((string-match "\\([^ ./,]+\\)\\([.][^( ]*\\)?[ ]*([ ]*L\\([0-9]+\\)[ ]*)" cur-line)
   ((string-match "\\([^ ./,]+\\)\\([.]\\([^( ]*\\)\\)?[ ]*([ ]*L\\([0-9]+\\)[ ]*)" cur-line)
    (let*  (
            ( tag  (substring cur-line (match-beginning 1) (match-end 1)))  ; tag without extension
            ( ext (cond                                                     ; extension if any
                   ((match-beginning 3)
                    (substring cur-line (match-beginning 3) (match-end 3)))
                   (t nil)))
            ( line (substring cur-line (match-beginning 4) (match-end 4)))  ; line number
            )

      (message (format "find-tag(%s) (L%s) '%s'" tag  line ext ) )

;        (message (format "find-tag(%s) (L%s) extension:'%s' match:%s file-name=%s" tag  line ext
;                         ( string-match (concat "[.]" ext "*$") (buffer-file-name)) (buffer-file-name) ) )

      (find-tag tag)                           ; Try to find the tag ...

      ;; If the tag have an extension (e.g. file.cc) that not match current buffer name,
      ;; try to get the correspond headerfile (e.g. file.hh)
      ;; But ee try to match the extension like ".extension*" of the file which sometimes occurs when
      ;; filenames are truncated like ... bcmcmiMain.c(L58)  but the real filename is bcmcmiMain.cc
      (if (and ext (not (string-match (concat "[.]" ext "*$") (buffer-file-name))))
          (ff-get-other-file))

      (goto-line (string-to-int line))         ; ... and go to the line
      ))
   )
)

(defun shell-jump ()
  "This function jumps to a or the shell buffer.

   If any prefix-arg (ctrl-u)
   has been given, a 'cd <current-file-name-dir> will be sent to current shell and the editor
   will be synchronized to <current-file-name-dir>.

   If the prefix-arg is given and current line contain a file path, you can jump to
   that directory where actual file are located. This feature can can typically be used
   in the *Buffer List* if you want to jump to the directory for actual file
   or in the *shell* buffer where you can jump on printouts of the unix command du etc."
  (interactive)

  (let*
      (
       ;; Current line containing filename etc.
       (cur-line (buffer-substring (progn (beginning-of-line) (point)) (progn (end-of-line) (point) )))
       )

    ;; Find a path that is suitable for a cd ...
    (setq buffer-dir
          (cond

           ;; Does current line contain a relative file path (./path) ?
           ((string-match "^[^.]+\\([.]/.*\\)[/ ]" cur-line)                ; Relative path: ./path
            (substring cur-line (match-beginning 1) (match-end 1)))

           ;; Does current line contain a complete file path ?
           ((string-match "^[^~]*\\(~/.*\\)/[^/]*$" cur-line)               ; Complete path ~/..
            (substring cur-line (match-beginning 1) (match-end 1)))

           ;; Does current line contain a complete file path ?
           ((string-match "^[^/]*\\(/.*\\)/[^/]*$" cur-line)                ; Complete path from the root
            (substring cur-line (match-beginning 1) (match-end 1)))

           ;; ..If not, use directory name for current buffer if defined
           ((buffer-file-name)
            (file-name-directory (buffer-file-name)))

           ;; ..else directory not defined
           (t nil)))
    )

  ;; Jump to another shell window
  (switch-to-buffer-other-window "*shell*")
  (shell)

  ;; Change directory of the remote shell if we have a path and
  ;; ctrl-u has been pressed before this command was invoked.
  (if (and buffer-dir current-prefix-arg)
      (shell-remote-cd buffer-dir))
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
;;(global-set-key [(f8)]            'list-buffers-same-window) ; List all buffers in a window
(global-set-key [(f8)]            'buffer-menu-other-window) ; List all buffers in a window
(global-set-key [(shift f8)]      'shell-jump)               ; Open or jump to shell buffer
(global-set-key [(f10)]           'ff-get-other-file)        ; Get corresponding .cc or .hh file
(global-set-key [(shift f10)]     'revert-buffer)            ; Refresh the buffer contents from file

(global-set-key "%"               'match-bracket)        ; Jump between scopes, simple (or just writing '%')
(global-set-key [(control ?5)]    'evilmi-jump-items)    ; Jump between scopes, fuller

(global-set-key [(meta +)]        'file-note-jump)           ; Jump to file:row
(global-set-key [(meta k)]        'copy-word)                ; Copy word to killbuffer and xclipboard

(global-set-key "\C-cc"           'compile)
(global-set-key "\C-c\C-c"        'make)

(global-set-key [(home)]          'beginning-of-buffer)
(global-set-key [(end)]           'end-of-buffer)

(global-set-key "\C-xg"           'magit-status)

(global-set-key [(meta up)]       'move-border-up)
(global-set-key [(meta down)]     'move-border-down)
(global-set-key [(meta left)]     'move-border-left)
(global-set-key [(meta right)]    'move-border-right)

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
 '(compilation-skip-threshold 2)
 '(indent-tabs-mode nil)
 '(line-number-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil))

(set-face-attribute 'default nil :height 100)

(setq initial-scratch-message "")  ;; Empty scratch-buffer message
(setq inhibit-startup-message t)   ;; No welcome screen

;; Change startup screen size
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 220))

;*********************************************************
(message "Loading ~/.emacs.d/init.el done")
;*********************************************************
