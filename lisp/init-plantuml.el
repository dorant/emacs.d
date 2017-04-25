(use-package plantuml-mode
  :ensure t
  :mode ("\\.p\\(lant\\)?uml\\'" . plantuml-mode)
  :config (progn
            (setq plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
            (setq plantuml-output-type "png")
            ;(setq plantuml-output-type "utxt")
            (unless (file-exists-p plantuml-jar-path)
              (alert (format "plantuml not found at %s" plantuml-jar-path)))))

;; (use-package flycheck-plantuml
;;   :ensure t
;;   :commands (flycheck-plantuml-setup)
;;   :init
;;   (with-eval-after-load 'flycheck
;;                         (flycheck-plantuml-setup)))

(provide 'init-plantuml)
