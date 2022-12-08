;; -*- lexical-binding: t -*-

;; -----------------------------------------------------------------------------
;; Defer garbage collection to later
(setq gc-cons-percentage-original gc-cons-percentage
      gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; -----------------------------------------------------------------------------
;; restore original gc values
(defun startup/reset-gc()
  (progn
    (setq gc-cons-threshold gc-cons-threshold-original)
    (setq gc-cons-percentage gc-cons-percentage-original)))

(add-hook 'emacs-startup-hook #'startup/reset-gc)

;; -----------------------------------------------------------------------------
;; silence compiler warnings
(setq warning-minimum-level :emergency)
(setq warning-suppress-types '((comp)))
(setq comp-async-report-warnings-errors nil)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local cl-functions))

;; -----------------------------------------------------------------------------
;; Prevent unwanted runtime compilation for gccemacs (native-comp)
;; users. packages are compiled ahead-of-time when they are installed
;; and site files are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil)

;; -----------------------------------------------------------------------------
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

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
(setq package-enable-at-startup nil)

;; -----------------------------------------------------------------------------
;; do not resize the frame at this early stage, it is quite an
;; expensive task.
(setq frame-inhibit-implied-resize t)

;; -----------------------------------------------------------------------------
;; once the config is loaded settings from our configuration will make
;; x-resources redundant. ignore it.
(advice-add #'x-apply-session-resources :override #'ignore)

;; -----------------------------------------------------------------------------
;; Emacs "updates" its ui more often than it needs to, so we slow it
;; down slightly from 0.5s
(setq idle-update-delay 1.0)

;; -----------------------------------------------------------------------------
;; don't want a mode line while loading init.
(setq mode-line-format nil)

;; -----------------------------------------------------------------------------
;; Disable bidirectional text scanning for a modest performance
;; boost. I've set this to `nil' in the past, but the
;; `bidi-display-reordering's docs say that is an undefined state and
;; suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; -----------------------------------------------------------------------------
;; Disabling the BPA makes redisplay faster, but might produce
;; incorrect display reordering of bidirectional text with embedded
;; parentheses and other bracket characters whose 'paired-bracket'
;; Unicode property is non-nil.
(setq bidi-inhibit-bpa t)

;; -----------------------------------------------------------------------------
;; miscellaneous optimizations
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq ring-bell-function 'ignore)

;; -----------------------------------------------------------------------------
;; remove unused gui toolkit components
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; -----------------------------------------------------------------------------
;; disable lots of annothing things that Emacs does at startup
(setq-default inhibit-splash-screen t                ; disable splash screen
              inhibit-startup-screen t               ; disable startup screen
              inhibit-startup-message t              ; disable startup message
              inhibit-startup-echo-area-message t    ; Disable initial echo message
              initial-scratch-message "")            ; Empty the initial *scratch* buffer

;; -----------------------------------------------------------------------------
;; early-init.el ends here
