;; -*- emacs-lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-initialize)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(show-paren-mode 1)
(transient-mark-mode t)
(global-font-lock-mode t)
(column-number-mode t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default cursor-in-non-selected-windows nil)
(setq visible-bell t)
(setq calendar-week-start-day 1)
(setq vc-follow-symlinks t)
(setq auto-revert-check-vc-info t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-xl" 'goto-line)
(global-set-key "\C-c\C-m" 'compile)
(global-set-key "\C-xS" 'term+mux-new)
(global-set-key "\C-x\C-o" 'find-file-other-window)
(global-set-key "\M-\C-s" 'helm-swoop)
(global-set-key "\C-t" 'pop-tag-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visual settings for X11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when window-system
  (set-face-attribute 'default nil
		      :family "Ricty diminished"
		      :height (if (equal (system-name) "x1carbon") 120 105))
  (set-fontset-font (frame-parameter nil 'font)
		    'japanese-jisx0208
		    (cons "Ricty diminished" "iso10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
		    'japanese-jisx0212
		    (cons "Ricty diminished" "iso10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
		    'katakana-jisx0201
		    (cons "Ricty diminished" "iso10646-1"))

  (set-face-attribute 'mode-line          nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (set-face-foreground 'mode-line-buffer-id nil)
  (set-face-background 'mode-line-buffer-id nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; color theme settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'wombat t)

(when (not window-system)
  (defun on-frame-open (&optional frame)
    "If the FRAME created in terminal don't load background color."
    (unless (display-graphic-p frame)
      (set-face-background 'default "unspecified-bg" frame)))
  (add-hook 'after-make-frame-functions 'on-frame-open)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for cmigemo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (setq migemo-command "cmigemo")
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (migemo-init)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bookmark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq bookmark-save-flag 1)

(progn
  (setq bookmark-sort-flag nil)
  (defun bookmark-arrange-latest-top ()
    (let ((latest ( bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (cons latest (delq latest bookmark-aliset))))
    (bookmark-save))
  (add-hook 'bookmark-after-jump-hook 'bookmark-arrange-latest-top))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linux C conding style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (c-set-style "linux-tabs-only")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook
          '(lambda ()
	     (c-set-style "linux")
	     ;(setq indent-tabs-mode nil)
	     ;(setq c-basic-offset 4)
             (auto-complete-mode)
	     ;(gtags-make-complete-list)
             ))

(add-hook 'lisp-mode
          '(lambda ()
             (setq indent-tabs-mode nil)
             ))

(add-hook 'sh-mode-hook
          '(lambda ()
             (setq indent-tabs-mode t)
             ))

(add-hook 'find-file-hook
	  '(lambda ()
	     (if (file-exists-p buffer-file-name)
		 (view-mode))
	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm-config)
(helm-mode 1)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(global-set-key (kbd "M-x") 'helm-M-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(setq ac-auto-show-menu nil)
(setq ac-use-quick-help nil)
(set-face-foreground 'ac-completion-face "black")
(set-face-background 'ac-completion-face "white")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for view-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar pager-keybind
  `(("h" . backward-char)
    ("l" . forward-char)
    ("j" . next-line)
    ("k" . previous-line)
    ("b" . scroll-down)
    ("\C-h" . scroll-down)
    (" " . scroll-up)
    ("n" . ,(lambda () (interactive) (scroll-up 1)))
    ("p" . ,(lambda () (interactive) (scroll-down 1)))
    ("q" . view-mode)
    ))

(defun define-many-keys (keymap key-table &optional includes)
  (let (key cmd)
    (dolist (key-cmd key-table)
      (setq key (car key-cmd)
            cmd (cdr key-cmd))
      (if (or (not includes) (member key includes))
          (define-key keymap key cmd))))
  keymap)

(add-hook 'view-mode-hook
          '(lambda ()
             (define-many-keys view-mode-map pager-keybind)
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for Mew
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'mew)
(global-set-key (kbd "C-x m") 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mozc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'mozc)
(setq default-input-method "japanese-mozc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elscreen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when window-system
  (require 'elscreen)
  (elscreen-start)
  (setq elscreen-display-tab t)
  (setq elscreen-tab-display-kill-screen nil) ;タブの先頭に[X]を表示しない
  (setq elscreen-tab-display-control nil) ;header-lineの先頭に[<->]を表示しない
  (global-set-key "\C-zl" 'elscreen-toggle)
  (global-set-key [C-tab] 'elscreen-toggle)
  (copy-face 'mode-line 'elscreen-tab-current-screen-face)
  (copy-face 'mode-line-inactive 'elscreen-tab-background-face)
  (copy-face 'mode-line-inactive 'elscreen-tab-control-face)
  (copy-face 'mode-line-inactive 'elscreen-tab-other-screen-face)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-startup-truncated nil)
(setq org-reverse-note-order t)
(setq org-agenda-files '("~/Dropbox/org/"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)" "DEFERRED(f)")
	(sequence "OPEN(o)" "|" "CLOSED(l)")
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aspell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'gtags)

(add-hook 'gtags-select-mode-hook
	  '(lambda ()
	     (setq hl-line-face 'underline)
	     (hl-line-mode 1)
	     ))

(add-hook 'c-mode-hook
	  '(lambda ()
	     (gtags-mode 1)))

(add-hook 'asm-mode-hook
	  '(lambda ()
	     (gtags-mode 1)))

(add-hook 'gtags-mode-hook
	  '(lambda ()
	     (local-set-key "\M-*" 'gtags-find-tag)
	     (local-set-key "\M-." 'gtags-find-tag-from-here)
	     (local-set-key "\M-r" 'gtags-find-rtag)
	     (local-set-key "\M-s" 'gtags-find-symbol)
	     (local-set-key "\C-t" 'gtags-pop-stack)
	     (local-set-key "\M-," 'gtags-pop-stack)
	     ))

(setq gtags-auto-update t)

;; disable helm in gtags functions
(add-to-list 'helm-completing-read-handlers-alist '(gtags-find-tag . nil))
(add-to-list 'helm-completing-read-handlers-alist '(gtags-find-rtag . nil))
(add-to-list 'helm-completing-read-handlers-alist '(gtags-find-symbol . nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq python-shell-interpreter "python3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flycheck)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
