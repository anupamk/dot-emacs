;; -----------------------------------------------------------------------------
;; starting with emacs-27.1, and early-init is now required to control
;; things with greater precision.
;;
;; concretely, these changes are due to how emacs initializes the
;; package manager. before emacs-27.1, init.el was responsible for
;; that task, via `package-initialize'. from emacs-27.1 onwards, the
;; default behvior is to start the package-manager before loading the
;; users init file.
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; do not initialise the package manager. this is done in `init.el'
(setq package-enable-at-startup t)

;; -----------------------------------------------------------------------------
;; allow loading from the package cache.
(setq package-quickstart t)

;; -----------------------------------------------------------------------------
;; do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; -----------------------------------------------------------------------------
;; early-init.el ends here
