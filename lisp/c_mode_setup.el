; Own C/C++ settings
; ==================

;;(defvar make-program "make"
;;  "Default program to run at `make'.")


;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(setq c-recognize-knr-p nil)

; Ellemtel c-style original - Don't change
;    ("ellemtel"
;     (c-basic-offset . 3)
;     (c-comment-only-line-offset . 0)
;     (c-hanging-braces-alist     . ((substatement-open before after)))
;     (c-offsets-alist . ((topmost-intro        . 0)
;                         (topmost-intro-cont   . 0)
;                         (substatement         . 3)
;	   		  (substatement-open    . 0)
;			  (statement-case-intro . 0)
;                         (case-label           . +)
;                         (access-label         . -3)
;                         (inclass              . 6)
;                         (inline-open          . 0)
;                         ))

; Emacs c-style (shall produce the same style as the Xemacs style below)
(defconst my-c-style
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

; Xemacs c-style
(defconst my-c-stylex
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
			(inline-open          . 0) ;  brace that opens an in-class inline method
                        (arglist-intro        . 0) ;  the first line in an argument list
                        (arglist-close        . 0) ;  the solo close paren of an argument list
			(label                . 0) ;  any non-special C/C++/ObjC label
			))
    )
  "My C Programming Style")
     


; --------------------------------------------------------
; Add my common hook to be called when entering emacsc/c++-mode
; --------------------------------------------------------
(add-hook 'c-mode-common-hook (function (lambda ()

  (message "c-mode-common-hook ...") 

;; Add my personal style and set it for the current buffer
;; (change current Ellemtel style instead of adding a new since APS-tools always selects Ellemtel-style)
  (if (string-match "XEmacs" emacs-version)
      (c-add-style "Ellemtel" my-c-stylex t)
    (c-add-style "Ellemtel" my-c-style t))

;  Indentation shall not insert real tabs 
  (setq indent-tabs-mode nil)

 ; Set C-style
  (c-set-style "Ellemtel")


; Preprocessor directives (used by the c-macro-expand command)
 

;   (setq c-macro-preprocessor "/usr/local/workshop3.0p/SUNWspro/SC4.2/bin/acomp -C -B")
;   (setq c-macro-preprocessor "/usr/bin/cpp -B")

   (setq c-macro-preprocessor "/usr/local/bin/cpp -C")

   (setq c-macro-cppflags     (concat 
                               "-I../include "
                               "-I../../include "
                               "-I/usr/include "
                               "-I/usr/include/linux "
                               "-I/usr/include/g++-3 "
                               "-I/usr/local/include "
                               "-I/usr/local/include/c++/3.3 "
                               "-I" (getenv "MY_CVS_HOME") "/vcl/export/include " 
                               "-I" (getenv "MY_CVS_HOME") "/chc/include " 
                               "-I" (getenv "MY_CVS_HOME") "/csi/include " 
                               "-I" (getenv "MY_CVS_HOME") "/smpp/export/include " 
                               "-I" (getenv "MY_CVS_HOME") "/etsi/include " 
                               "-I" (getenv "MY_CVS_HOME") "/gsm0340/include " 
                               "-I" (getenv "MY_CVS_HOME") "/smptp/include " 
                               "-I" (getenv "MY_CVS_HOME") "/cimd/include " 
                               "-I" (getenv "MY_CVS_HOME") "/tcom/include " 
                               "-I" (getenv "MY_CVS_HOME") "/vcg/include " 
                               "-I" (getenv "MY_CVS_HOME") "/vcm/include " 
                               "-I" (getenv "MY_CVS_HOME") "/vpsp/export/include " 
                               "-I" (getenv "MY_CVS_HOME") "/vpstn/include " 
                               "-I" (getenv "MY_CVS_HOME") "/rpp/include " 
                               "-I" (getenv "MY_CVS_HOME") "/cont/include " 
                               "-I" (getenv "MY_CVS_HOME") "/rpi/include " 
                               "-I" (getenv "MY_CVS_HOME") "/im/include " 
                               "-I" (getenv "MY_CVS_HOME") "/sfs/include " 
                               "-I" (getenv "MY_CVS_HOME") "/sfc/include " 
                               "-I" (getenv "MY_CVS_HOME") "/cdrp/include " 
                               "-I" (getenv "MY_CVS_HOME") "/cdb/include " 
                               "-I" (getenv "MY_CVS_HOME") "/tsa/include " 
                               "-I" (getenv "MY_CVS_HOME") "/cdr/include " 
                               "-I" (getenv "MY_CVS_HOME") "/tts/include " 
                               "-I" (getenv "MY_CVS_HOME") "/alarm/include " 
                               "-I" (getenv "MY_CVS_HOME") "/gsmmodem/include " 
                               "-I" (getenv "MY_CVS_HOME") "/license/include " 
                               "-D_REENTRANT"

                               ))

)))

; --------------------------------------------------------
; Add my hook to be called when entering c-mode.
; --------------------------------------------------------
(add-hook 'c-mode-hook (function (lambda ()

  (message "c-mode-hook ...") 

  ; Bind make key
  (define-key c-mode-map "\C-c\C-c"  'make)
)))


; --------------------------------------------------------
; Add my hook to be called when entering any c++-mode.
; --------------------------------------------------------
(add-hook 'c++-mode-hook (function (lambda ()

  (message "c++-mode-hook ...") 

  ; Bind make key
  (define-key c++-mode-map "\C-c\C-c"  'make)
)))

; --------------------------------------------------------
; Add my common hook to be called when entering a makefile
; --------------------------------------------------------
(add-hook 'makefile-mode-hook (function (lambda ()

  (message "makefile-mode-hook ...")  

  ; Bind make key
  (define-key makefile-mode-map "\C-c\C-c"  'make)  
)))




; --------------------------------------------------------
; Useful functions
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

(defun cmake () (interactive)
  (if
    (and
       (not (eq (buffer-file-name) nil))
       (buffer-modified-p)
    )
    (save-buffer)
  )

  (let ((tags (if current-prefix-arg  (read-input "cmake :" "clean") "")))
    (compile (concat "cmake " tags))
    )
)


(defun clearmake () (interactive)
  (if
    (and
       (not (eq (buffer-file-name) nil))
       (buffer-modified-p)
    )
    (save-buffer)
  )


  (let ((tags (if current-prefix-arg  (read-input "clearmake :" "clean") "")))
    (compile (concat "clearmake " tags))
    )
)
