; disable scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
; disable toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
; disable menu
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
; turn off splash screen
(setq inhibit-splash-screen t)
; start with an empty buffer
(switch-to-buffer (get-buffer-create "Empty"))
;(delete-other-windows)
(setq line-number-mode t)
(setq column-number-mode t)
;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq ediff-split-window-function 'split-window-horizontally)
(setq delete-selection-mode t)

(setq make-backup-files nil)
; enable inter-program clipboard
(setq x-select-enable-clipboard t)
;
(setq-default tab-width 4)
; set tab to be the tab
(setq-default indent-tabs-mode t)

(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-h" 'backward-kill-word)
(global-set-key "\M-p" 'scroll-down)
(global-set-key "\M-n" 'scroll-up)
(global-set-key "\C-xg" 'goto-line)
(global-set-key "\C-xr" 'replace-string)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

(setq compile-command "make")

; cua-mode - cut,copy,paste
; delete-selection-mode

;;;----------------------------------------
;;; extra packages path
;;;----------------------------------------
(add-to-list 'load-path "~/.emacs.d/")

;;;----------------------------------------
;;; erlang-mode
;;;----------------------------------------

;(setq erlang-root-dir (concat (getenv "ERL_ROOT") "/lib/erlang"))
(setq erlang-root-dir "/opt/otp-r15b01/lib/erlang")
(setq load-path (cons (car (file-expand-wildcards (concat erlang-root-dir "/lib/tools-*/emacs"))) load-path))
(setq erlang-electric-commands nil)
(require 'erlang-start)

(add-hook 'erlang-mode-hook
  '(lambda()
	 (setq tab-width 4)
	 (setq tab-stop-list
		'(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
	 (setq indent-tabs-mode t)
	 (setq indent-line-function 'insert-tab)

	 (imenu-add-to-menubar "Imenu")

	 (local-set-key (kbd "C-c C-m m") 'erlang-man-module) ; M-F1
	 (local-set-key (kbd "C-c C-m f") 'erlang-man-function))); M-F2

; define name and cookie for internally loaded erlang shell.
(setq inferior-erlang-machine-options
	  '("-name" "emacs@127.0.0.1" "-setcookie" "emacs"))

; define auto erlang mode for these files/extensions.
(add-to-list 'auto-mode-alist '(".*\\.app\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*app\\.src\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.config\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.rel\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.script\\'" . erlang-mode))
;(add-to-list 'auto-mode-alist '(".*rebar\\.config\\'" . erlang-mode))
;(add-to-list 'auto-mode-alist '(".*reltool\\.config\\'" . erlang-mode))
;(add-to-list 'auto-mode-alist '(".*app\\.config\\'" . erlang-mode))
;(add-to-list 'auto-mode-alist '(".*sys\\.config\\'" . erlang-mode))

; add include directory to default compile path.
(defvar erlang-compile-extra-opts
  '(bin_opt_info debug_info (i . "../include") (i . "../deps") (i . "../../") (i . "../../../deps")))

; define where put beam files.
(setq erlang-compile-outdir "../ebin")

;;;----------------------------------------
;;; flymake
;;;----------------------------------------

(require 'flymake)
(require 'flymake-cursor) ; http://www.emacswiki.org/emacs/FlymakeCursor
(setq flymake-log-level 3)

(defun flymake-compile-script-path (path)
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
					 'flymake-create-temp-inplace))
		 (local-file (file-relative-name
					  temp-file
					  (file-name-directory buffer-file-name))))
	(list path (list local-file))))

(defun flymake-syntaxerl ()
  (flymake-compile-script-path "~/bin/syntaxerl"))

(add-hook 'erlang-mode-hook
  '(lambda()
	 (local-set-key (kbd "C-c C-f d") 'flymake-display-err-menu-for-current-line) ; M-F1
	 (local-set-key (kbd "C-c C-f n") 'flymake-goto-next-error) ; F3
	 (local-set-key (kbd "C-c C-f p") 'flymake-goto-prev-error) ; M-F3

	 (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-syntaxerl))
	 (add-to-list 'flymake-allowed-file-name-masks '("\\.hrl\\'" flymake-syntaxerl))
	 (add-to-list 'flymake-allowed-file-name-masks '("\\.app\\'" flymake-syntaxerl))
	 (add-to-list 'flymake-allowed-file-name-masks '("\\.app.src\\'" flymake-syntaxerl))
	 (add-to-list 'flymake-allowed-file-name-masks '("\\.config\\'" flymake-syntaxerl))
	 (add-to-list 'flymake-allowed-file-name-masks '("\\.rel\\'" flymake-syntaxerl))
	 (add-to-list 'flymake-allowed-file-name-masks '("\\.script\\'" flymake-syntaxerl))

	 ;; should be the last.
	 (flymake-mode 1)
))

; see /usr/local/lib/erlang/lib/tools-<Ver>/emacs/erlang-flymake.erl
(defun erlang-flymake-only-on-save ()
  "Trigger flymake only when the buffer is saved (disables syntax
check on newline and when there are no changes)."
  (interactive)
  ;; There doesn't seem to be a way of disabling this; set to the
  ;; largest int available as a workaround (most-positive-fixnum
  ;; equates to 8.5 years on my machine, so it ought to be enough ;-) )
  (setq flymake-no-changes-timeout most-positive-fixnum)
  (setq flymake-start-syntax-check-on-newline nil))

(erlang-flymake-only-on-save)

;;;----------------------------------------
;;; wrangler
;;; http://www.cs.kent.ac.uk/projects/wrangler/Home.html
;;;----------------------------------------

(add-to-list 'load-path "/usr/local/lib/erlang/lib/wrangler-1.0/elisp")
(require 'wrangler)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(wrangler-search-paths (quote ("/home/ten0s/Projects/wrangler"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;;----------------------------------------
;;; scheme-mode
;;; http://www.cs.rpi.edu/academics/courses/fall05/ai/scheme/starting.html
;;;----------------------------------------

(load-library "xscheme")

; sets emacs default directory. probably should be on of the latest command in .emacs
(setq default-directory "~/Projects")

;;;----------------------------------------
;;; ask before closing emacs
;;;----------------------------------------

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
	  (save-buffers-kill-emacs)
      (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))
