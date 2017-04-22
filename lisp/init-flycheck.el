(use-package flycheck)

;; Turn flycheck on everywhere
(global-flycheck-mode)

;; Force flycheck to always use c++11 support. We use
;; the clang language backend so this is set to clang
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-language-standard "c++11")))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
