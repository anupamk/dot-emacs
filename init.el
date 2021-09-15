;; -*- lexical-binding: t; no-byte-compile: t -*-

;; -----------------------------------------------------------------------------
;; this is the bootstrap file which is loaded when emacs
;; starts. subsequently it loads `emacs-init.el' which is generated
;; from the `emacs-init.org' where the bulk of my emacs configuration
;; lives.
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; initialize + enable package-manager
(require 'package)

(setq package-archives '(("org"    . "http://orgmode.org/elpa/")
                         ("gnu"    . "http://elpa.gnu.org/packages/")
                         ("melpa"  . "http://melpa.org/packages/")
                         ))

;; initialize the packages, avoiding re-initialization
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup t)
  (package-initialize))

;; make sure that `use-package' is available.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; -----------------------------------------------------------------------------
;; configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil)

  ;; ---------------------------------------------------------------------------
  ;; write hooks using their real name instead of a shorter version:
  ;; after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

;; -----------------------------------------------------------------------------
;; GCMH - the Garbage Collector Magic Hack
;;
;; Enforce a sneaky Garbage Collection strategy to minimize GC
;; interference with the activity. During normal usage a high GC
;; threshold is set. When idling, GC is immediately triggered and a
;; low threshold is set.
(use-package gcmh
  :ensure
  :delight
  :init
  (setq gcmh-verbose             t
        gcmh-lows-cons-threshold (* 16 1024 1024)  ; 16mb
        gcmh-high-cons-threshold (* 128 1024 1024) ; 128mb
        gcmh-idle-delay          5)
  :config
  (gcmh-mode 1))

;; for my custom libraries
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; -----------------------------------------------------------------------------
;; bootstrap is over, continue loading the real configuration
(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-init.org") t)
