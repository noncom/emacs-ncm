;;; Originally based on Emfy 0.4.0 <https://github.com/susam/emfy>

;;; ------------------------------------------------------------------
;;; 	Look and Feel
;;; ------------------------------------------------------------------

;; Tweak UI.
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 1)
  (menu-bar-mode 0))
(setq inhibit-startup-screen t)
(column-number-mode)

;; Dark theme.
(load-theme 'wombat)
(with-eval-after-load 'wombat-theme
  (set-face-background 'default "#111")
  (set-face-background 'cursor "#c96")
  (set-face-foreground 'font-lock-comment-face "#fc0")
  (set-face-background 'isearch "#ff0")
  (set-face-foreground 'isearch "#000")
  (set-face-background 'lazy-highlight "#990")
  (set-face-foreground 'lazy-highlight "#000"))

;; Enable line numbers while writing config, code, or text.
(dolist (hook '(prog-mode-hook conf-mode-hook text-mode-hook))
  (add-hook hook 'display-line-numbers-mode))

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; Auto-complete inputs in the minibuffer.
(fido-vertical-mode)

;; Fonts, from https://www.reddit.com/r/emacs/comments/xybrtw/what_is_the_most_appropriate_way_to_set_fonts_in/ 
(defun font-exists-p (font) (if (null (x-list-fonts font)) nil t))
(when (window-system)
  (cond ((font-exists-p "Consolas") (set-frame-font "Consolas:spacing=100:size=14" nil t))
    ((font-exists-p "Droid Sans") (set-frame-font "Droid Sans:spacing=100:size=14" nil t))))

;;; ------------------------------------------------------------------
;;; 	Whitespace
;;; ------------------------------------------------------------------

;; Show trailing whitespace while writing config, code, or text.
(dolist (hook '(conf-mode-hook prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace t))))

;; Show stray blank lines.
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Add a newline automatically at the end of a file while saving.
(setq require-final-newline t)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 4 characters wide.
(setq-default tab-width 4)

;; Indentation setting for various languages.
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq css-indent-offset 2)

;;; ------------------------------------------------------------------
;;; 	Clean Working Directories
;;; ------------------------------------------------------------------

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)

;; Write customizations to a separate file instead of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;;; ------------------------------------------------------------------
;;;     Custom Command and Key Sequences
;;; ------------------------------------------------------------------

;; Custom command.
(defun show-current-time ()
  "Show current time."
  (interactive)
  (message (current-time-string)))

(defun open-user-init-file ()
  "Open user init file"
  (interactive)
  (find-file user-init-file))

;; Custom key sequences.
(global-set-key (kbd "C-c t") 'show-current-time)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)

(global-set-key (kbd "C-c u i") 'open-user-init-file)

;;; ------------------------------------------------------------------
;;; 	Emacs Server
;;; ------------------------------------------------------------------

(require 'server)
(unless (server-running-p)
  (server-start))

;;; ------------------------------------------------------------------
;;; 	Package Setup
;;; ------------------------------------------------------------------

(defun install-packages ()
  "Install and set up packages for the first time."
  (interactive)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents)
  (dolist (package '(slime
                     markdown-mode
                     paredit
                     rainbow-delimiters))
    (unless (package-installed-p package)
      (package-install package))))

;;; ------------------------------------------------------------------
;;; 	Paredit Configuration
;;; ------------------------------------------------------------------

;; Enable Paredit on various Lisp modes.
(when (fboundp 'paredit-mode)
  (dolist (hook '(emacs-lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  ielm-mode-hook
                  lisp-interaction-mode-hook
                  lisp-mode-hook
		  slime-repl-mode-hook))
    (add-hook hook 'enable-paredit-mode)))

;; Do not bind RET to paredit-RET which prevents input from being
;; evaluated on RET in M-:, ielm, etc.
(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "RET") nil))

;;; ------------------------------------------------------------------
;;;	    Slime Configuration
;;; ------------------------------------------------------------------

(defun override-slime-del-key ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'override-slime-del-key)

;; Configure SBCL as the Lisp program for SLIME.
(add-to-list 'exec-path "/usr/local/bin")
(setq inferior-lisp-program "sbcl")

;;; ------------------------------------------------------------------
;;; 	Rainbow Delimiters Configuration
;;; ------------------------------------------------------------------

(when (fboundp 'rainbow-delimiters-mode)
  (dolist (hook '(emacs-lisp-mode-hook
                  ielm-mode-hook
                  lisp-interaction-mode-hook
                  lisp-mode-hook))
    (add-hook hook 'rainbow-delimiters-mode)))

(with-eval-after-load 'rainbow-delimiters
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666")) ; dark gray

;;; -----------------------------------------------------------------
;;; 	Fennel
;;; -----------------------------------------------------------------

;; (defvar fennel-path (expand-file-name "../../_addons/fennel-mode/fennel-mode" (file-name-directory load-file-name)))
(defvar this-file-dir (file-name-directory load-file-name))
(defvar fennel-path (expand-file-name "_plugins/fennel-mode/fennel-mode" this-file-dir))

(message "[plugin] fennel-path = \"%s\"" fennel-path)

(autoload 'fennel-mode fennel-path nil t)
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

(defvar fennel-ls-path (expand-file-name "_apps/fennel-ls/run_fennel_ls.bat" this-file-dir))

(with-eval-after-load 'eglot
  (message "[app] About to hook fennel mode to LSP at \"%s\"" fennel-ls-path)
  (add-to-list 'eglot-server-programs `(fennel-mode . (,fennel-ls-path)))
  (message "[app] Hooking complete"))

;;; ------------------------------------------------------------------
;;; 	The End
;;; ------------------------------------------------------------------

(provide 'init)

;;; ------------------------------------------------------------------
;;; 	init.el ends here
;;; ------------------------------------------------------------------
