;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; *********************************************************
;;;
;;;  Initialization file for Emacs
;;;
;;; *********************************************************
;;; Code:

(message "Loading ~/.emacs.d/init.el ...")

;; --------------------------------------------------------
;; Package handling
;; --------------------------------------------------------

(if (locate-library "package")
    (eval-and-compile
      (setq package-enable-at-startup nil
            package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                               ("melpa"        . "https://melpa.org/packages/")
                               ("gnu"          . "https://elpa.gnu.org/packages/"))
            use-package-always-pin "melpa"
            use-package-verbose t
            tls-checktrust t
            tls-program '("gnutls-cli --x509cafile %t -p %p %h")
            gnutls-verify-error t)
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

;; Ensure environment variables looks the same in Emacs as in the shell
(use-package exec-path-from-shell
  :ensure t
  :if (string-equal system-type "darwin")
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "RUST_SRC_PATH"))
  (exec-path-from-shell-initialize))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 90
        auto-package-update-prompt-before-update t)
  (auto-package-update-maybe))

;; --------------------------------------------------------
;; Initiate visuals
;; --------------------------------------------------------

;; Themes and powerline (i.e. infobar)
(use-package init-visuals
  :load-path "lisp/")

;; Zoom, inc/dec text size
(use-package default-text-scale
  :ensure t
  :pin melpa
  :bind (("C-0" . default-text-scale-reset)
         ("C--" . default-text-scale-decrease)
         ("C-=" . default-text-scale-increase)))

;; Functions for moving buffers relative its position
(use-package move-border
  :load-path "lisp/"
  :bind (([(meta up)]    . move-border-up)
         ([(meta down)]  . move-border-down)
         ([(meta left)]  . move-border-left)
         ([(meta right)] . move-border-right)))

;; Prefer window-split: top-bottom
(setq split-width-threshold nil)
;;(setq split-height-threshold nil)

;; --------------------------------------------------------
;; Initiate helpers and modes
;; --------------------------------------------------------

;; Dont create lockfiles for files that is currently edited .#xxx
(setq create-lockfiles nil)

;; Make sure flymake stays quiet
(flymake-mode -1)
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)


;; Jump between matching tags
(use-package evil-matchit
  :ensure t
  :bind ("C-%" . evilmi-jump-items)
  :config
  (global-evil-matchit-mode 1))

;; Find File At Point
(use-package ffap
  :config
  (ffap-bindings)) ; Default key bindings

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
  (ido-mode 1))

;; ;; M-x enhancement that uses ido
;; (use-package smex
;;   :ensure t
;;   :config (smex-initialize)
;;   :bind ("M-x" . smex))

(use-package tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh"))
;;        trap-verbose 9
;;        tramp-ssh-controlmaster-options

(use-package vagrant-tramp
  :ensure t)

;; ;; NOT READY: Find using rtags, or else tags
;; (defun my-find-symbol (next-p)
;;   (interactive "P")
;;   (or (and (require 'rtags nil t)
;;            (rtags-find-symbol-at-point))
;;       (and (progn (setq tag (symbol-name (symbol-at-point)))
;;                   (message (format "Could not find using rtags, attempt tags for finding '%s'" tag))
;;                   )
;;            (condition-case nil
;;                (find-tag tag next-p)
;;              (error nil)))))


;; --------------------------------------------------------
;; Common language tools
;; --------------------------------------------------------
(use-package projectile
  :ensure t
  ;; :init
  ;; (setq projectile-keymap-prefix (kbd "C-c C-p"))
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
; LSP uses projectile to find root

;; provides fancier overlays (like helptext)
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;; :config (progn
;;           ;; disable inline documentation
;;           (setq lsp-ui-sideline-enable nil)
;;           ;; disable showing docs on hover at the top of the window
;;           (setq lsp-ui-doc-enable nil))
;; )

;; ;; Optional - provides snippet support.
;; (use-package yasnippet
;;   :ensure t
;;   :commands yas-minor-mode
;;   :hook (go-mode . yas-minor-mode))

(use-package lsp-treemacs
  :ensure t)

;; Completion
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind ([C-tab] . company-complete)
  :config
  (setq company-idle-delay nil ; avoid autocompletion popup, use C+TAB
        company-minimum-prefix-length 2
        company-show-numbers nil
        company-tooltip-limit 15
        company-dabbrev-downcase nil
        ;;company-backends '((company-irony))
        )

  ;; Sort in previously used order
  (use-package company-statistics
    :ensure t
    :config
    (add-hook 'after-init-hook #'company-statistics-mode))

  ;; ;; Add help window
  ;; (use-package company-quickhelp
  ;;   :ensure t
  ;;   :if window-system
  ;;   :config
  ;;   (company-quickhelp-mode)
  ;;   (setq company-quickhelp-delay 0))

  ;; Fuzzy matching
  (use-package company-flx
    :ensure t
    :config
    (company-flx-mode t))
)

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :defer)

(use-package lsp-mode
  :ensure t
  ;; uncomment to enable gopls http debug server
  ;; :custom (lsp-gopls-server-args '("-debug" "127.0.0.1:0"))
  :commands (lsp lsp-deferred)
  :hook ((before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports)))

;; Hide mode info from modeline when requested
(use-package delight
  :ensure t
  :pin melpa-stable)

(use-package flycheck
  :ensure t
  :delight
  :config (global-flycheck-mode))

;; Spellchecking
(use-package flycheck-vale
  :ensure t
  :defer t
  :after flycheck
  :config (flycheck-vale-setup))

;; --------------------------------------------------------
;; Go  (Search-tag: golang)
;; --------------------------------------------------------

;; Improved replacement for electric-pair-mode
(defun my/go-electric-brace ()
  "Insert an opening brace may be with the closing one.
If there is a space before the brace also adds new line with
properly indented closing brace and moves cursor to another line
inserted between the braces between the braces."
  (interactive)
  (if (not (looking-back " "))
      (insert "{")
    (insert "{")
    (newline)
    (indent-according-to-mode)
    (save-excursion
      (newline)
      (insert "}")
      (indent-according-to-mode))))

(defun my/indent-or-complete ()
  "Complete or indent if nothing to complete."
  (interactive)
  (if (and (looking-at "\\s-\\|$")      ;; Whitespace or end of line
           (looking-back "\\w\\|\\."))  ;; and previous is word or dot
      (company-complete)
    (indent-for-tab-command)))

(defun my/go-mode-hook ()
  "Customize compile command to run go build."
  (setq compile-command (concat "echo Building... && go build -v && "
                                "echo Testing... && go test -v && "
                                "echo Linters... && golint && go vet")))

(use-package go-guru
  :ensure t
  :after go-mode)

(use-package go-mode
  :ensure t
  :init (setq go-fontify-function-calls nil)
  ;; :init (setq gofmt-command "goimports")
  :bind (:map go-mode-map
              ("{" . my/go-electric-brace) ;; Auto-add end-brace
              ("<tab>" . my/indent-or-complete))
  :hook ((go-mode . lsp-deferred)
         (go-mode . my/go-mode-hook)))

;; (use-package company-go
;;   :ensure t
;;   :config
;;   (setq company-go-show-annotation nil)) ;; Set to t for showing args in popup

;; (use-package go-eldoc
;;   :ensure t
;;   :init
;;   (add-hook 'go-mode-hook 'go-eldoc-setup))

;; (use-package flycheck-gometalinter
;;   :ensure t
;;   :config
;;   (progn
;;     (flycheck-gometalinter-setup)))


;; --------------------------------------------------------
;; Rust
;; --------------------------------------------------------
(use-package toml-mode
  :ensure t)

(use-package rustic
  :ensure t
  :init
  (setq rustic-format-trigger 'on-save)
  (setq rustic-lsp-server 'rust-analyzer)
  ;(setq lsp-prefer-flymake nil)
  ;(rustic-flycheck-setup-mode-line-p nil)
  ;; :config
  ;; (setq lsp-enable-snippet nil)
  ;; (setq lsp-rust-rls-server-command '("rustup" "run" "nightly" "rls"))
  )

;; DEPRICATED !!
;;;(use-package lsp-rust)

;; ;; ;; Add keybindings for interacting with Cargo
;; ;; (use-package cargo
;; ;;   :ensure t
;; ;;   :hook (rust-mode . cargo-minor-mode))

;; (use-package rust-mode
;;   :ensure t
;; ;  :diminish cargo-minor-mode
;; ;  :hook (rust-mode . lsp)
;;   :config
;;   (setq rust-format-on-save t)
;;   (setq lsp-rust-rls-server-command '("rustup" "run" "nightly" "rls")))

;; ;; (use-package flycheck-rust
;; ;;   :ensure t
;; ;;   :hook (flycheck-mode . flycheck-rust-setup))

;; ;; (use-package company
;; ;;   :ensure t
;; ;;   :hook (prog-mode . company-mode)
;; ;;   :config (setq company-tooltip-align-annotations t)
;; ;;           (setq company-minimum-prefix-length 1))








;; ;; ;; rustup self update
;; ;; ;; rustup show
;; ;; ;; # Set in ~/.bash_profile
;; ;; ;; export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/src
;; ;; ;; rustup toolchain add nightly
;; ;; ;; rustup component add rust-src
;; ;; ;; cargo +nightly install racer
;; ;; ;; rustup component add rustfmt

;; ;; (use-package rust-mode
;; ;;   :ensure t
;; ;;   :mode "\\.rs\\'"
;; ;;   ;;:init
;; ;;   ;;(global-company-mode)

;; ;;   :config
;; ;;   ;; (use-package company-racer)
;; ;;   ;; (use-package flycheck-rust)
;; ;;   (use-package racer
;; ;;     :ensure t
;; ;;     :hook (rust-mode . racer-mode)
;; ;;     :config
;; ;;     (define-key rust-mode-map (kbd "M-\"") #'racer-find-definition)
;; ;;     (add-hook 'racer-mode-hook #'eldoc-mode)
;; ;;     (add-hook 'racer-mode-hook #'company-mode)
;; ;;     (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
;; ;;     (setq company-tooltip-align-annotations t)
;; ;;     )
;; ;;   (defun my-rust-mode-hook()
;; ;;     ;; (set (make-local-variable 'compile-command "cargo run"))
;; ;;     (setq compile-command "cargo +nightly run")
;; ;;     (setq rust-format-on-save t)
;; ;;     (setq rust-match-angle-brackets nil) ;; https://github.com/rust-lang/rust-mode/issues/288
;; ;;     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;; ;;     ;;(set (make-local-variable 'company-backends) '(company-racer))
;; ;;     ;;(local-set-key (kbd "TAB") #'racer-complete-or-indent)
;; ;;     )
;; ;;   (add-hook 'rust-mode-hook 'my-rust-mode-hook)
;; ;;   )


;; --------------------------------------------------------
;; C / C++
;; --------------------------------------------------------
(defun my/flycheck-rtags-hook ()
  (flycheck-mode)
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-check-syntax-automatically nil) ;; Let rtags trigger the check
  (setq-local flycheck-highlighting-mode nil) ;; Use rtags own overlay/highlighting
  )

(use-package rtags
  ;; :load-path "~/bin/rtags/share/emacs/site-lisp/rtags/"
  :config
  (setq rtags-autostart-diagnostics t
        rtags-completions-enabled t)
  (rtags-enable-standard-keybindings)
  (add-hook 'c++-mode-hook #'my/flycheck-rtags-hook)
;;  (add-to-list 'company-backends 'company-rtags)
;;  (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running))

  ;; (define-key c-mode-base-map (kbd "M-.")
  ;;    (function rtags-find-symbol-at-point))
  ;; (define-key c-mode-base-map (kbd "M-,")
  ;;    (function rtags-find-references-at-point))
  ;; (define-key c-mode-base-map (kbd "M-*")
  ;;    (function rtags-location-stack-back))
  ;; (define-key c-mode-base-map (kbd "M-/")
  ;;    (function rtags-find-all-references-at-point))
  )

(use-package flycheck-rtags
  :ensure t)


(defun my/irony-mode-hook ()
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
  (add-hook 'irony-mode-hook #'my/irony-mode-hook)
  ;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

;; Define the coding style
(defconst my/cc-style
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
  "My C/C++ Programming Style.")

(use-package cc-mode
  :commands c++-mode
  :mode ("\\.h\\\'" . c++-mode) ;; Let .h files be opened in C++ mode
  :config
  (setq c-recognize-knr-p nil) ;; Dont check for old-style (K&R) function declarations, this should speed up indenting
  (c-add-style "my/cc-style" my/cc-style)

  (add-hook 'c-mode-hook (function (lambda ()
                                     (define-key c-mode-map "\C-c\C-c" 'make)
				     (c-set-style "my/cc-style"))))
  (add-hook 'c++-mode-hook (function (lambda ()
                                       (define-key c++-mode-map "\C-c\C-c" 'make)
                                       (c-set-style "my/cc-style"))))
  (add-hook 'makefile-mode-hook (function (lambda ()
                                            (define-key makefile-mode-map "\C-c\C-c" 'make)
                                            (message "makefile-mode-hook ..."))))
)

; --------------------------------------------------------
; Save buffer and compile
; --------------------------------------------------------
(defun make ()
  "Run a make program in current dir by invoking: `make-program'."
  (interactive)
  (if (and (not (eq (buffer-file-name) nil)) (buffer-modified-p))
      (save-buffer)
    )

  (let ((tags (if current-prefix-arg  (read-input (concat make-program " ") "clean") "")))
    (compile (concat make-program " " tags))
    )
  )

; --------------------------------------------------------
; Syntax highlighting support for "Modern C++" - until C++17
; --------------------------------------------------------
(use-package modern-cpp-font-lock
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  (add-hook 'c-mode-hook   #'modern-c++-font-lock-mode))


(use-package fill-column-indicator
  :ensure t
  :if (>= emacs-major-version 25)
  :init
  (setq fci-rule-column 80)
  (setq fci-rule-color "#073642")
  (setq fci-rule-width 2))

;;  (add-hook 'markdown-mode-hook (lambda ()  (setq fci-rule-column 80)))

(defun my/fci-enabled ()
  (and (boundp 'fci-mode) fci-mode))

(defvar my/fci-mode-suppressed nil)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible."
  (let ((fci-enabled (my/fci-enabled)))
    (when fci-enabled
      (set (make-local-variable 'my/fci-mode-suppressed) fci-enabled)
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed."
  (when (and my/fci-mode-suppressed
             (null popup-instances))
    (setq my/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

(defadvice enable-theme (after recompute-fci-face activate)
  "Regenerate fci-mode line images after switching themes."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (turn-on-fci-mode))))

(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (if (or (or (derived-mode-p 'prog-mode) (eq major-mode 'text-mode)) (eq major-mode 'markdown-mode))
                 (fci-mode 1))))

;; fci seems to distort company-mode graphics
;;(global-fci-mode 1)


;; --------------------------------------------------------
;; Erlang
;; --------------------------------------------------------
(use-package ivy-erlang-complete
  :ensure t
  :pin melpa-stable)

(use-package company-erlang
  :ensure t
  :pin melpa-stable)

(use-package erlang
  :ensure t
  :load-path (lambda () (substitute-in-file-name "${_KERL_ACTIVE_DIR}/lib/tools-3.3/emacs/"))
  ;; :hook (before-save . erlang-indent-current-buffer)
  :hook (after-save . ivy-erlang-complete-reparse)
  :custom (ivy-erlang-complete-erlang-root (substitute-in-file-name "${_KERL_ACTIVE_DIR}/"))
  :config (ivy-erlang-complete-init)
  :mode (("\\.erl?$" . erlang-mode)
	 ("rebar\\.config$" . erlang-mode)
	 ("relx\\.config$" . erlang-mode)
	 ("sys\\.config\\.src$" . erlang-mode)
	 ("sys\\.config$" . erlang-mode)
	 ("\\.config\\.src?$" . erlang-mode)
	 ("\\.config\\.script?$" . erlang-mode)
	 ("\\.hrl?$" . erlang-mode)
	 ("\\.app?$" . erlang-mode)
	 ("\\.app.src?$" . erlang-mode)
	 ("\\Emakefile" . erlang-mode)
         ("elvis\\.config$" . erlang-mode))
  :config
  (setq erlang-indent-level 4))

;; --------------------------------------------------------
;; Sh
;; --------------------------------------------------------
(add-hook 'sh-mode-hook 'flycheck-mode)


;;----------------------------------------------------------------------------
;; Other modes
;;----------------------------------------------------------------------------
;; Emacs mode for editing Cucumber plain text stories
;; https://github.com/michaelklishin/cucumber.el
(use-package feature-mode
  :ensure t
  :defer t
  :mode ("\\.feature$"))

;; Terraform
(use-package terraform-mode
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

;; PlantUML
(use-package plantuml-mode
  :ensure t
  :mode ("\\.p\\(lant\\)?u\\(ml\\)?\\'")
  :config (progn
            (setq plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
            (setq plantuml-output-type "png")
            ;(setq plantuml-output-type "utxt")
            (unless (file-exists-p plantuml-jar-path)
              (message (format "plantuml not found at %s" plantuml-jar-path)))))

(use-package yaml-mode
  :ensure t)

(use-package groovy-mode
  :ensure t
  :mode ("Jenkinsfile.*\\'" . groovy-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Docker
(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile.*\\'")

(use-package docker-tramp
  :ensure t)

;; CMake color
(use-package cmake-mode
  :ensure t)

;; Git
(use-package magit
  :ensure t
  :bind ("\C-xg" . magit-status))

;; (use-package magit-gerrit
;;   :ensure t)

;; HTML
(use-package web-mode
  :ensure t
  :mode
  (
   ".html?$"
   ".jsp$"
   ".vue$"
   )
  :config
  (setq
   web-mode-markup-indent-offset 4
   web-mode-css-indent-offset 4
   web-mode-code-indent-offset 4
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing t
   web-mode-enable-auto-indentation t
   ))

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

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    ;; (use-package treemacs-evil
    ;;   :ensure t
    ;;   :demand t)
    (setq treemacs-change-root-without-asking nil
          treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-never-persist              nil
          treemacs-no-png-images              nil
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple)))))

(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))


(use-package asn1-mode
  :load-path "lisp/asn1-mode/")

;;======================================
;; *shell*
;; https://ambrevar.bitbucket.io/emacs-eshell/
;; https://emacs.stackexchange.com/questions/37887/send-region-to-shell-in-another-buffer
;; https://gist.github.com/ugovaretto/0339c5e3efe3fdde502a
;;======================================
;; Avoid break in shell
(setenv "PAGER" "cat")

(setq explicit-shell-file-name "/bin/bash")
(setq comint-scroll-to-bottom-on-output t) ;; Autoscroll to bottom
(setq comint-process-echoes nil)
;; No trailing whitespace in shell
(add-hook 'shell-mode-hook
          (lambda () (setq-local show-trailing-whitespace nil)))
;; No trailing whitespace in Buffer menu
(add-hook 'buffer-menu-mode-hook
	  (lambda () (setq-local show-trailing-whitespace nil)))

(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1"
       "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])

(setq ansi-color-map (ansi-color-make-color-map))

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
  (add-hook 'compilation-filter-hook #'colorize-compilation-buffer))
  ;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  ;; (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on))



(defun send-current-line-to-process (arg beg end)
  "Send the current line to a process buffer.
   The first time it's called, will prompt for the buffer to
   send to. Subsequent calls send to the same buffer, unless a
   prefix argument is used (C-u), or the buffer no longer has an
   active process."
  (interactive "P\nr")

  ;; Handle process selection
  (when (or arg ;; user asks for selection
            (not (boundp 'sendline-buffer-target)) ;; target not set
            ;; or target is not set to an active process:
            (not (process-live-p (get-buffer-process sendline-buffer-target))))
    (let (procs buf)
      (setq procs (remove nil (seq-map
                               (lambda (el)
                                 (when (setq buf (process-buffer el))
                                   (buffer-name buf)))
                               (process-list))))
      (if (not procs) (error "No process buffers currently open.")
        (setq sendline-buffer-target (completing-read "Process: " procs)))))

  ;; Send current line
  (let ((currentline (concat (buffer-substring (save-excursion (beginning-of-line) (point))
                                       (save-excursion (end-of-line) (point))) "\n")))

    ;; Echo currentline to buffer
    (with-current-buffer sendline-buffer-target
      (goto-char (process-mark (get-buffer-process sendline-buffer-target)))
      (insert currentline)
      (move-marker (process-mark (get-buffer-process sendline-buffer-target)) (point)))

    (message (format "send-line: %s" currentline))
    (process-send-string sendline-buffer-target currentline))

  ;; Show buffer
  (display-buffer sendline-buffer-target t)
  ;; Jump to next line
  (next-line))


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
(defvar make-program "make -f ~/Makefile"
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

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "New name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "New directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	  (if (string-match dir "\\(?:/\\|\\\\)$")
	      (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil)
             t))))

;;----------------------------------------------------------------------------
;; Key setup
;;----------------------------------------------------------------------------

; MacOSX bindings
(if (string-equal system-type "darwin")
    (progn
      (message "Mac OSX key-bindings ...")
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


(global-set-key [(f1)]            'other-window)               ; Jump between windows
(global-set-key [(shift f1)]      'goto-line)                  ; Goto line in file
(global-set-key [(f2)]            'indent-for-comment)         ; Comment line after (C/C++)
(global-set-key [(f3)]            'comment-region)             ; Comment a market region
(global-set-key [(shift f3)]      'uncomment-region)           ; Uncomment a market region
(global-set-key [(f4)]            'next-error)                 ; Next error in a compiler result
(global-set-key [(f6)]            'treemacs-projectile)        ; Show files with treemacs
(global-set-key [(shift f6)]      'treemacs-projectile-toggle) ; Show files with treemacs
(global-set-key [(f8)]            'buffer-menu-other-window)   ; List all buffers in a window
(global-set-key [(shift f8)]      'shell-jump)                 ; Open or jump to shell buffer

(global-set-key [(f9)]            'send-current-line-to-process) ; tcl-send-current-line)    ; Send command to a process
;; (global-set-key [(shift f9)]      'tcl-eval-region-new)
;; (global-set-key [(control f9)]    'tcl-source-current-function)
;; (global-set-key [(control shift f9)] 'tcl-set-current-process-buffer-new) ; Associate a text file with a process for F9 command sending

(global-set-key [(f10)]           'ff-get-other-file)          ; Get corresponding .cc or .hh file
(global-set-key [(shift f10)]     'revert-buffer)              ; Refresh the buffer contents from file

(global-set-key "%"               'match-bracket)              ; Jump between scopes, simple (or just writing '%')

(global-set-key [(meta +)]        'file-note-jump)             ; Jump to file:row
(global-set-key [(meta k)]        'copy-word)                  ; Copy word to killbuffer and xclipboard

(global-set-key "\C-cc"           'compile)
(global-set-key "\C-c\C-c"        'recompile)
;;(global-set-key "\C-c\C-c"        'make)

(global-set-key [(home)]          'beginning-of-buffer)
(global-set-key [(end)]           'end-of-buffer)

;;Seems missing in Emacs 25.X
(global-set-key [(meta *)]        'pop-tag-mark)

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
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output 'first-error)
 '(compilation-skip-threshold 2)
 '(frame-background-mode 'dark)
 '(indent-tabs-mode nil)
 '(line-number-mode t)
 '(package-selected-packages
   '(company-erlang ivy-erlang-complete projectile yasnippet lsp-mode flycheck-rust cargo toml-mode hcl-mode terraform-mode treemacs-projectile treemacs flycheck-gometalinter go-eldoc company-go go-mode fill-column-indicator modern-cpp-font-lock plantuml-mode magit cmake-mode company-flx company-quickhelp company-statistics company irony flycheck-vale flycheck-rtags flycheck markdown-mode groovy-mode yaml-mode dockerfile-mode docker smex evil-matchit color-theme-solarized color-theme exec-path-from-shell use-package))
 '(show-trailing-whitespace t)
 '(solarized-termcolors 256)
 '(tool-bar-mode nil))
;;----------------------------------------------------------------------------

(when (file-exists-p "~/.emacs.local.el")
  (load "~/.emacs.local.el"))

;;*********************************************************
(message "Loading ~/.emacs.d/init.el done")
;;*********************************************************
(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
