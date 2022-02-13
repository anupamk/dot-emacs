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
;; if native compilation is available, automatically generate compiled
;; files when Emacs loads a new .el file. this *will* freeze Emacs for
;; a while until it's done.
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (progn
      (message "Native compilation is available")

      ;; -----------------------------------------------------------------------
      ;; automatically generate compiled files when Emacs loads a new .elc
      ;; file, it *will* freeze Emacs for a while until it's done.
      (setq comp-deferred-compilation t)

      ;; -----------------------------------------------------------------------
      ;; whether to query user about killing async compilations on
      ;; exit.
      ;;    when non-nil => emacs asks for confirmation (and kill
      ;;    async-compilations if any are running)
      ;;
      ;;    when nil => emacs will silently kill those
      ;;    async-compilations iff `confirm-kill-processes' is
      ;;    non-nil
      (setq native-comp-async-query-on-exit t)

      ;; -----------------------------------------------------------------------
      ;; default number of subprocesses used for
      ;; async-native-compilation, when '0' ==> use half number of cpu
      ;; execution units.
      (setq native-comp-async-jobs-number 0)

      ;; -----------------------------------------------------------------------
      ;; Whether to report warnings and errors from asynchronous
      ;; native compilation.
      (setq native-comp-async-report-warnings-errors nil)

      ;; -----------------------------------------------------------------------
      ;; when non-nil => native compile packages on installation.
      (setq package-native-compile t)))

;; -----------------------------------------------------------------------------
;; bootstrap is over, continue loading the real configuration
(require 'org)

;; -----------------------------------------------------------------------------
;; return true when modification-time attribute of fname is older than 'time'
(defun anupamk/file-is-older-than-time-p (fname time)
  "Return 't' when modification-time attribute of 'FNAME' is older than
'TIME', 'nil' otherwise.

'TIME' is expected to be in the same format as output from (current-time)"
  (let* ((time-sec (time-convert time 'integer))
         (filemod-time-sec (time-convert (file-attribute-modification-time (file-attributes fname)) 'integer)))
    (< filemod-time-sec time-sec)))

;; -----------------------------------------------------------------------------
;; return true when modification-time attribute of fname is older than 'time'
(defun anupamk/file-is-older-than-time-p (fname time)
  "Return 't' when modification-time attribute of 'FNAME' is older than
'TIME', 'nil' otherwise.

'TIME' is expected to be in the same format as output from (current-time)"
  (let* ((time-sec (time-convert time 'integer))
         (filemod-time-sec (time-convert (file-attribute-modification-time (file-attributes fname)) 'integer)))
    (message (format "'%s' modification-time: '%d', compare-time: '%d'"
                     fname
                     filemod-time-sec
                     time-sec))
    (< filemod-time-sec time-sec)))

;; -----------------------------------------------------------------------------
;; return 'true' when modification-time attribute of 'fname-1' is
;; older than 'fname-2'
(defun anupamk/file-is-older-than-file-p (fname-1 fname-2)
  "Return 't' when modification-time attribute of 'FNAME-1' is older than
modification-time attribute of 'FNAME-2', 'nil' otherwise."
  (when (and (file-exists-p fname-1)
             (file-exists-p fname-2))
    (anupamk/file-is-older-than-time-p fname-1
                                       (file-attribute-modification-time (file-attributes fname-2)))))

;; -----------------------------------------------------------------------------
;; byte-compile emacs-init.el generated from emacs-init.org
(defun anupamk/build-emacs-config()
  "Generate `emacs-init.el' from `emacs-init.org'.

This function is added to the `kill-emacs-hook' and, if required, it generates a
new `emacs-init.elc' from `emacs-init.org' when Emacs session is terminated."
  (interactive)
  (let* ((emacs-init-org-fname "~/.emacs.d/emacs-init.org")
         (emacs-init-el-fname (concat (file-name-sans-extension emacs-init-org-fname) ".el")))

    (when (or (not (file-exists-p emacs-init-el-fname))
              (anupamk/file-is-older-than-file-p emacs-init-el-fname emacs-init-org-fname))
      (org-babel-tangle-file emacs-init-org-fname emacs-init-el-fname)
      (byte-compile-file emacs-init-el-fname))
    (message (format "'%s' is current, nothing to do !!!" emacs-init-el-fname))))

(add-hook 'kill-emacs-hook #'anupamk/build-emacs-config)

;; -----------------------------------------------------------------------------
;; load the emacs-init.org or byte-compiled version of it produced
;; from running `anupamk-build-emacs-config'
(defun anupamk/load-emacs-config()
  (interactive)
  (if (file-exists-p "~/.emacs.d/emacs-init.el")
      (load-file "~/.emacs.d/emacs-init.el")
    (when (file-exists-p "~/.emacs.d/emacs-init.org")
      (progn (anupamk/build-emacs-config)
             (anupamk/load-emacs-config)))))

;; -----------------------------------------------------------------------------
;; load the config
(anupamk/load-emacs-config)
