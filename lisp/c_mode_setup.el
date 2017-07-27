; Own C/C++ settings
; ==================

;; Let .h files be opened in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(setq c-recognize-knr-p nil)

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
  "My C Programming Style")
(c-add-style "my-cc-style" my-cc-style)


; --------------------------------------------------------
; Add my hook to be called when entering c-mode.
; --------------------------------------------------------
(add-hook 'c-mode-hook (function (lambda ()

  (message "c-mode-hook ...")

  ; Bind make key
  (define-key c-mode-map "\C-c\C-c"  'make)

  (c-set-style "my-cc-style")
)))


; --------------------------------------------------------
; Add my hook to be called when entering any c++-mode.
; --------------------------------------------------------
(add-hook 'c++-mode-hook (function (lambda ()

  (message "c++-mode-hook ...")

  ; Bind make key
  (define-key c++-mode-map "\C-c\C-c"  'make)

  (c-set-style "my-cc-style")
)))

; --------------------------------------------------------
; Add my common hook to be called when entering a makefile
; --------------------------------------------------------
(add-hook 'makefile-mode-hook (function (lambda ()

  ; Bind make key
  (define-key makefile-mode-map "\C-c\C-c"  'make)

  (message "makefile-mode-hook ...")
)))


; --------------------------------------------------------
; Helper functions
; --------------------------------------------------------
(defun make ()
  "Runs a make program in current dir by invoking: `make-program'"
  (interactive)
  (if
      (and
       (not (eq (buffer-file-name) nil))
       (buffer-modified-p)
       )
      (save-buffer)
    )

  (let ((tags (if current-prefix-arg  (read-input (concat make-program " ") "clean") "")))
    (compile (concat make-program " " tags))
    )
)
