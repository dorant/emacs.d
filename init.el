;;*********************************************************
;;
;; Description
;;  Setup file for emacs
;;
;;*********************************************************
(message "Loading ~/.emacs.d/init.el ...")
;;*********************************************************

;; --------------------------------------------------------
;; Package handling
;; --------------------------------------------------------

(if (locate-library "package")
    (eval-and-compile
      (setq package-enable-at-startup nil
            package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                               ("melpa"        . "https://melpa.org/packages/")
                               ("gnu"          . "https://elpa.gnu.org/packages/"))
            use-package-always-pin "melpa-stable"
            use-package-verbose t)
      (require 'package)
      (package-initialize)
      (unless (package-installed-p 'use-package)
        (unless (assoc 'use-package package-archive-contents)
          (package-refresh-contents))
        (package-install 'use-package))
      (require 'use-package))
  (message "WARNING: Ancient emacs! No advice-add, package.el")
  (defmacro advice-add (&rest body))
  (defmacro use-package (&rest body)))


;; --------------------------------------------------------
;; Initiate helpers and modes
;; --------------------------------------------------------

;; Themes and powerline (i.e. infobar)
(use-package init-themes
  :load-path "lisp/")

;; Jump between matching tags
(use-package evil-matchit
  :ensure t
  :bind ("C-%" . evilmi-jump-items)
  :config
  (global-evil-matchit-mode 1))

;; Functions for moving buffers relative its position
(use-package move-border
  :load-path "lisp/"
  :bind (([(meta up)]       . move-border-up)
         ([(meta down)]     . move-border-down)
         ([(meta left)]     . move-border-left)
         ([(meta right)]    . move-border-right)))


;; Find File At Point
(use-package ffap
  :config
  (ffap-bindings) ; Default key bindings
  )

;; Proposals in minibuffer
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-auto-merge-work-directories-length nil
        ido-use-filename-at-point 'guess
        ido-use-url-at-point t
        ffap-require-prefix t)
  (ido-mode 1)
  )

;; M-x enhancement that uses ido
(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind ("M-x" . smex))

(use-package tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh"))
;;        trap-verbose 9
;;        tramp-ssh-controlmaster-options

;; Docker
(use-package docker
  :ensure t
  :pin melpa
  :config
  (docker-global-mode))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile.*\\'")

(use-package docker-tramp
  :ensure t)



;; NOT READY: Find using rtags, or else tags
(defun my-find-symbol (next-p)
  (interactive "P")
  (or (and (require 'rtags nil t)
           (rtags-find-symbol-at-point))
      (and (progn (setq tag (symbol-name (symbol-at-point)))
                  (message (format "Could not find using rtags, attempt tags for finding '%s'" tag))
                  )
           (condition-case nil
               (find-tag tag next-p)
             (error nil)))
      )
  )


(use-package rtags
  :load-path "~/bin/rtags/share/emacs/site-lisp/rtags/"
  :config
  (setq rtags-autostart-diagnostics t
        rtags-completions-enabled t)
  (rtags-enable-standard-keybindings)
;;  (add-to-list 'company-backends 'company-rtags)
;;  (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running))

  (define-key c-mode-base-map (kbd "M-.")
     (function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,")
     (function rtags-find-references-at-point))
  (define-key c-mode-base-map (kbd "M-*")
     (function rtags-location-stack-back))
  (define-key c-mode-base-map (kbd "M-/")
     (function rtags-find-all-references-at-point))
)

;;(require 'init-python-mode)           ;; Python mode

(defun my-flycheck-rtags-hook ()
  (flycheck-mode)
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-check-syntax-automatically nil) ;; Let rtags trigger the check
  (setq-local flycheck-highlighting-mode nil) ;; Use rtags own overlay/highlighting
  )

(use-package flycheck
  :ensure t
  :init
  (add-hook 'c++-mode-hook #'my-flycheck-rtags-hook)

  :config
  (use-package flycheck-rtags
    :ensure t)
  )



(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(use-package irony
  :ensure t
  :init
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'c-mode-hook #'irony-mode)
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (add-hook 'irony-mode-hook #'my-irony-mode-hook)
  ;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

;; Completion
(use-package company
  :ensure t
  :bind ([C-tab] . company-complete)
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-idle-delay nil
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-limit 15
        company-dabbrev-downcase nil
        ;;company-backends '((company-irony))
	)

  ;; Sort in previously used order
  (use-package company-statistics
    :ensure t
    :config
    (add-hook 'after-init-hook #'company-statistics-mode))
  ;; Add help window
  (use-package company-quickhelp
    :ensure t
    :if window-system
    :config
    (company-quickhelp-mode)
    (setq company-quickhelp-delay 0.6))
  ;; Fuzzy matching
  (use-package company-flx
    :ensure t
    :pin melpa
    :config
    (company-flx-mode t))
)


;; CMake color
(use-package cmake-mode
  :ensure t)

;; Git
(use-package magit
  :ensure t
  :bind ("\C-xg" . magit-status))


;; Define the coding style
(defconst my-cc-style
  '((c-tab-always-indent           . t)
    (c-basic-offset                . 4)
    (c-comment-only-line-offset    . 0)
    (c-hanging-braces-alist (substatement-open before after))
    (c-offsets-alist . ((topmost-intro        . 0) ;  the first line in a topmost construct definition
                        (topmost-intro-cont   . 0) ;  topmost definition continuation lines
                        (substatement         . 4) ;  the first line after an if/while/for/do/else
                        (substatement-open    . 0) ;  the brace that opens a substatement block
                        (statement-case-intro . 4) ;  the first line in a case `block'
                        (case-label           . +) ;  a case or default label
                        (access-label         . -4);  C++ private/protected/public access label
                        (inclass              . 4) ;  the construct is nested inside a class definition
                        (innamespace          . 0) ;  used inside C++ namespace constructs
                        (inline-open          . 0) ;  brace that opens an in-class inline method
                        (arglist-intro        . 0) ;  the first line in an argument list
                        (arglist-close        . 0) ;  the solo close paren of an argument list
                        (label                . 0) ;  any non-special C/C++/ObjC label
                        ))
    )
  "My C/C++ Programming Style")

(use-package cc-mode
  :commands c++-mode
  :mode ("\\.h\\\'" . c++-mode) ;; Let .h files be opened in C++ mode
  :config
  (setq c-recognize-knr-p nil) ;; Dont check for old-style (K&R) function declarations, this should speed up indenting
  (c-add-style "my-cc-style" my-cc-style)

  (add-hook 'c-mode-hook (function (lambda ()
                                     (define-key c-mode-map "\C-c\C-c" 'make)
				     (c-set-style "my-cc-style"))))
  (add-hook 'c++-mode-hook (function (lambda ()
                                       (define-key c++-mode-map "\C-c\C-c" 'make)
                                       (c-set-style "my-cc-style"))))
  (add-hook 'makefile-mode-hook (function (lambda ()
                                            (define-key makefile-mode-map "\C-c\C-c" 'make)
                                            (message "makefile-mode-hook ..."))))
)

; --------------------------------------------------------
; Save buffer and compile
; --------------------------------------------------------
(defun make ()
  "Runs a make program in current dir by invoking: `make-program'"
  (interactive)
  (if (and (not (eq (buffer-file-name) nil)) (buffer-modified-p))
      (save-buffer)
    )

  (let ((tags (if current-prefix-arg  (read-input (concat make-program " ") "clean") "")))
    (compile (concat make-program " " tags))
    )
  )


(use-package plantuml-mode
  :ensure t
  :mode ("\\.p\\(lant\\)?uml\\'")
  :config (progn
            (setq plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
            (setq plantuml-output-type "png")
            ;(setq plantuml-output-type "utxt")
            (unless (file-exists-p plantuml-jar-path)
              (alert (format "plantuml not found at %s" plantuml-jar-path)))))


; --------------------------------------------------------
; Syntax highlighting support for "Modern C++" - until C++17
; --------------------------------------------------------
(use-package modern-cpp-font-lock
  :ensure t
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
  (add-hook 'compilation-filter-hook #'colorize-compilation-buffer)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)
  )

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(use-package server
  :ensure t
  :demand
  :init
  (server-mode 1)
  :config
  (unless (server-running-p)
    (server-start)))

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
;; Utilities
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
(global-set-key [(f8)]            'buffer-menu-other-window) ; List all buffers in a window
(global-set-key [(shift f8)]      'shell-jump)               ; Open or jump to shell buffer
(global-set-key [(f10)]           'ff-get-other-file)        ; Get corresponding .cc or .hh file
(global-set-key [(shift f10)]     'revert-buffer)            ; Refresh the buffer contents from file

(global-set-key "%"               'match-bracket)            ; Jump between scopes, simple (or just writing '%')

(global-set-key [(meta +)]        'file-note-jump)           ; Jump to file:row
(global-set-key [(meta k)]        'copy-word)                ; Copy word to killbuffer and xclipboard

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
 '(compilation-skip-threshold 2)
 '(indent-tabs-mode nil)
 '(line-number-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;*********************************************************


;;(set-face-attribute 'default nil :height 100)

(setq initial-scratch-message "")  ;; Empty scratch-buffer message
(setq inhibit-startup-message t)   ;; No welcome screen

;; Change startup screen size
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 220))

;;*********************************************************
(message "Loading ~/.emacs.d/init.el done")
;;*********************************************************
