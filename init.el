;; -*- lexical-binding: t; -*-

;; -----------------------------------------------------------------------------
;; this is the bootstrap file which is loaded when emacs
;; starts. subsequently it loads `emacs-init.el' which is generated
;; from the `emacs-init.org' where the bulk of my emacs configuration
;; lives.
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; make startup faster.
;;
;; some background: the variable `file-name-handler-alist' specifies
;; an alist of elements (regexp . handler) for file names handled
;; specially. if a file name matches the regexp, all i/o on that file
;; is done by calling the handler.
;;
;; during startup we don't care about these. once startup is
;; over, we restore things back.
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  "Revert file name handler alist."
  (setq file-name-handler-alist startup/file-name-handler-alist))

(add-hook 'emacs-startup-hook #'startup/revert-file-name-handler-alist)

;; -----------------------------------------------------------------------------
;; maximum number of bytes (4mb) to read from sub-process in a single
;; chunk.
(setq read-process-output-max (* 4 1024 1024))

;; -----------------------------------------------------------------------------
;; initialize + enable package-manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; -----------------------------------------------------------------------------
;; some perf tweaks
(setq read-process-output-max (* 4 1024 1024)) ; 4mb
(setq process-adaptive-read-buffering nil)     ; performance

;; -----------------------------------------------------------------------------
;; install use-package, and configure it to use straight.el by default
(straight-use-package 'use-package)

(use-package straight
  :custom

  ;; ---------------------------------------------------------------------------
  ;; by default, `use-package' uses package.el to install
  ;; packages.
  ;;
  ;; however, we want to ensure that `use-package' uses straight.el by
  ;; default for that.
  (straight-use-package-by-default t)

  ;; ---------------------------------------------------------------------------
  ;; use the gnu-elpa mirror instead of the original, for reasons that
  ;; are best appreciated by readong the 'GNU ELPA` section of
  ;; straight.el
  (straight-recipes-gnu-elpa-use-mirror t)

  ;; ---------------------------------------------------------------------------
  ;; is the package modified ?
  (straight-check-for-modifications nil)
  )

;; -----------------------------------------------------------------------------
;; but we still want to peruse the melpa archives via the canonical
;; `package-list-packages', enable that.
(require 'package)
(setq package-archives '(("org"    . "http://orgmode.org/elpa/")
                         ("gnu"    . "http://elpa.gnu.org/packages/")
                         ("melpa"  . "http://melpa.org/packages/")
                         ))

;; -----------------------------------------------------------------------------
;; load newer .elc or .el
(setq load-prefer-newer t)

;; -----------------------------------------------------------------------------
;; load the org-mode from elpa/melpa rather than the builtin
(straight-use-package 'org)

;; -----------------------------------------------------------------------------
;; GCMH - the Garbage Collector Magic Hack
;;
;; Enforce a sneaky Garbage Collection strategy to minimize GC
;; interference with the activity. During normal usage a high GC
;; threshold is set. When idling, GC is immediately triggered and a
;; low threshold is set.
(use-package gcmh
  :init
  (setq gcmh-verbose             t
        gcmh-lows-cons-threshold (* 16 1024 1024)  ; 16mb
        gcmh-high-cons-threshold (* 128 1024 1024) ; 128mb
        gcmh-idle-delay          5)
  :config
  (gcmh-mode 1)
  )

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

;; ------------------------------------------------------------------------------
;; the main configuration is a 'literate configuration' in an org
;; file.
;;
;; via the canonical usage of 'local variables' the elisp sources from
;; the org file is tangled into emacs-init.el
;;
;; this file is then loaded here...
(defvar main-config-fname "emacs-init.el"
  "basename of main configuration file")

(defvar main-config-el-filename
  (concat user-emacs-directory main-config-fname)
  "full pathname of main emacs configuration file")

(when (file-readable-p main-config-el-filename)
  (load-file main-config-el-filename))

;; -----------------------------------------------------------------------------
;; init.el ends here
