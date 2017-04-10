
(use-package flycheck)
(use-package auto-complete)
(use-package auto-complete-clang)
(use-package company)
(use-package irony)
(use-package cmake-ide)

(require 'rtags) ;; optional, must have rtags installed
(cmake-ide-setup)

;;(setq cmake-ide-build-dir "/repo/qbjsven/epg/build/Linux_x86_64.clangdebug/")

(provide 'init-cmake-ide)
