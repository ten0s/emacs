(set-scroll-bar-mode nil) ; nil | 'left | 'right
(tool-bar-mode -1) ; neg - disable | pos - enable
(menu-bar-mode t) ; nil | t
; turn off splash screen
(setq inhibit-splash-screen t)
; start with an empty buffer
(switch-to-buffer (get-buffer-create "Empty"))
;(delete-other-windows)
(setq line-number-mode t)
(setq column-number-mode t)
; make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)
(setq show-trailing-whitespace t)
(show-paren-mode t)
;(setq show-paren-style 'expression)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq ediff-split-window-function 'split-window-horizontally)
(delete-selection-mode 1) ; neg - disable | pos - enable
(setq make-backup-files nil)
; enable inter-program clipboard
(setq x-select-enable-clipboard t)
; set tab to be the tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-h" 'backward-kill-word)

(global-set-key "\C-xr" 'replace-string)
(global-set-key "\M-\C-xr" 'replace-regexp)

(global-set-key "\C-x\C-g" 'find-grep)

;;; disable suspend-frame combinations
(global-set-key "\C-z" nil)
(global-set-key "\C-x\C-z" nil)

(setq compile-command "make")

;;; make the new script executable after writing.
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

;;;----------------------------------------
;;; fast completion modes
;;;----------------------------------------

; after you press M-x, available completions are listed as you type.
(icomplete-mode t)

;; do not confirm a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)

(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-enable-last-directory-history nil)
(setq ido-auto-merge-work-directories-length nil)
(setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
(setq ido-show-dot-for-dired t) ;; put . as the first item
(setq ido-use-filename-at-point nil)

;;;----------------------------------------
;;; extra packages path
;;;----------------------------------------

(add-to-list 'load-path "~/.emacs.d/")

;;;----------------------------------------
;;; dired mode
;;;----------------------------------------

(add-hook 'dired-mode-hook
	'(lambda()
	   ; C-c C-c or C-x C-s (wdired-finish-edit)
	   ; C-c ESC (wdired-abort_changes)
	   (local-set-key "\C-c\C-q" 'wdired-change-to-wdired-mode)
))

;;;----------------------------------------
;;; erlang-mode
;;;----------------------------------------

;(setq erlang-root-dir (getenv "_KERL_ACTIVE_DIR"))
(setq erlang-root-dir "/opt/r16b03-1")
(setq load-path (cons (car (file-expand-wildcards (concat erlang-root-dir "/lib/tools-*/emacs"))) load-path))
(setq erlang-electric-commands nil)
(require 'erlang-start)

(add-hook 'erlang-mode-hook
  '(lambda()
	 (setq tab-width 4)
	 (setq tab-stop-list
		'(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
	 (setq indent-tabs-mode nil)
	 (setq indent-line-function 'insert-tab)

	 (imenu-add-to-menubar "Imenu")

	 (local-set-key "\C-cm" 'erlang-man-module)
	 (local-set-key "\C-cf" 'erlang-man-function)

	 (if (not tags-table-list)
		 (setq tags-table-list '("~/.emacs.d")))

	 ; M-. find tag
	 ; M-0 M-. find next tag
	 ; C-u M-. find next tag
	 ; M-* pop back
	 ; M-x visit-tags-table
	 ; M-x tags-search
	 ; tags-file-name
	 ; tags-table-list
))

; define name and cookie for internally loaded erlang shell.
(setq inferior-erlang-machine-options
	  '("-name" "emacs@127.0.0.1" "-setcookie" "emacs" "-pa" "../ebin"))

; define auto erlang mode for these files/extensions.
(add-to-list 'auto-mode-alist '(".*\\.app\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*app\\.src\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.config\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.rel\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.script\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.escript\\'" . erlang-mode))
;(add-to-list 'auto-mode-alist '(".*rebar\\.config\\'" . erlang-mode))
;(add-to-list 'auto-mode-alist '(".*reltool\\.config\\'" . erlang-mode))
;(add-to-list 'auto-mode-alist '(".*app\\.config\\'" . erlang-mode))
;(add-to-list 'auto-mode-alist '(".*sys\\.config\\'" . erlang-mode))

; add include directory to default compile path.
(defvar erlang-compile-extra-opts
  '(bin_opt_info debug_info (d . \'TEST\') (i . "../include") (i . "../deps") (i . "../deps/*/ebin") (i . "../../") (i . "../../../deps")))

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
	 ; it's not actual any more due to flymake-cursor
	 (local-set-key "\C-cd" 'flymake-display-err-menu-for-current-line)
	 (local-set-key "\C-cn" 'flymake-goto-next-error)
	 (local-set-key "\C-cp" 'flymake-goto-prev-error)

	 (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-syntaxerl))
	 (add-to-list 'flymake-allowed-file-name-masks '("\\.hrl\\'" flymake-syntaxerl))
	 (add-to-list 'flymake-allowed-file-name-masks '("\\.app\\'" flymake-syntaxerl))
	 (add-to-list 'flymake-allowed-file-name-masks '("\\.app.src\\'" flymake-syntaxerl))
	 (add-to-list 'flymake-allowed-file-name-masks '("\\.config\\'" flymake-syntaxerl))
	 (add-to-list 'flymake-allowed-file-name-masks '("\\.rel\\'" flymake-syntaxerl))
	 (add-to-list 'flymake-allowed-file-name-masks '("\\.script\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.escript\\'" flymake-syntaxerl))

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
;;; distel
;;;----------------------------------------

;(add-to-list 'load-path "~/.emacs.d/share/distel/elisp")
;(require 'distel)
;(distel-setup)

;;;----------------------------------------
;;; wrangler
;;; http://www.cs.kent.ac.uk/projects/wrangler/Home.html
;;;----------------------------------------

;; (add-to-list 'load-path (concat erlang-root-dir "/lib/wrangler-1.0/elisp"))
;; (require 'wrangler)

;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(wrangler-search-paths (quote ("/home/ten0s/projects/temp/wr"))))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  )

; sets emacs default directory. probably should be on of the latest command in .emacs
(setq default-directory "~/")

;;;----------------------------------------
;; plantuml-mode
;;; https://github.com/zwz/plantuml-mode
;;;----------------------------------------

;(setq plantuml-jar-path "/opt/PlantUML/plantuml.jar")
;(require 'plantuml-mode)

;;;----------------------------------------
;;; ask before closing emacs
;;;----------------------------------------

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
	  (save-buffers-kill-emacs)
      (message "Canceled exit")))

(global-set-key "\C-x\C-c" 'ask-before-closing)

;;;----------------------------------------
;;; backward kill line
;;;----------------------------------------

;; define the function to kill the characters from the cursor
;; to the beginning of the current line
(defun backward-kill-line (arg)
  "Kill the characters from the cursor to the beginning of the current line"
  (interactive "p")
  (kill-line 0))

;(global-set-key "\C-u" 'backward-kill-line)

;;;----------------------------------------
;;;
;;;----------------------------------------

(defun prettify-code ()
  "Remove spaces between the text and parentheses,
make sure there's a space between a semicolon and the
text after it"
  (interactive)
  (setq pairs '(
				("\\([{[(]\\)\\s-+\\([^ ]\\)" . "\\1\\2") ; [{(space+char -> [{(char
				; unfortunately I can't make }]) work as above
				("\\([^ ]\\)\\s-+}" . "\\1}") ; charspace+} -> char}
				("\\([^ ]\\)\\s-+]" . "\\1]") ; charspace+] -> char]
				("\\([^ ]\\)\\s-+)" . "\\1)") ; charspace+) -> char)
				(",\\([^ ]\\)" . ", \\1") ; ,char -> ,spacechar
				))
  (mapcar '(lambda (pair)
			 (let ((from (car pair))
				   (to (cdr pair)))
			   (beginning-of-buffer)
			   (replace-regexp from to)))
		  pairs)
  (beginning-of-buffer)
  (keyboard-quit))

;(while (re-search-forward "\\([0-9]+\\)\\." nil t)
;                        (replace-match "\\1,"))

;;;----------------------------------------
;;;
;;;----------------------------------------

(defun insert-caption (title)
  (interactive "sTitle: ")
  (insert "%% ===================================================================\n")
  (insert (format "%%%% %s\n" title))
  (insert "%% ===================================================================\n"))

(add-hook 'desktop-save-hook 'tramp-cleanup-all-buffers)
