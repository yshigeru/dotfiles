;; -*- emacs-lisp -*-
(mapcar '(lambda (dir)
           (if (not (member dir load-path))
               (setq load-path (cons dir load-path))))
        '("~/.emacs.d/auto-install" "~/.emacs.d"))

(setenv "PATH" (concat "/sbin:/usr/sbin:"
		       "/opt/powerpc-devel/bin:"
		       (getenv "PATH")))
;(setenv "LANG" "C")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when window-system
  (set-default-font "SourceCodePro-9")
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-robin-hood)
  
  (set-face-attribute 'mode-line          nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (set-face-foreground 'mode-line-buffer-id nil)
  (set-face-background 'mode-line-buffer-id nil)

  (setq initial-frame-alist
	(append '((width . 120) (height . 60)) initial-frame-alist))
  )

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
(global-linum-mode)

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'xcscope)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-xl" 'goto-line)
(global-set-key "\C-c\C-m" 'compile)
(global-set-key "\C-xS" 'multi-shell-new)
(global-set-key "\C-xT" 'multi-term)

;; Linux C conding style
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

;; hooks
(add-hook 'c-mode-common-hook
          '(lambda ()
	     (c-set-style "linux")
	     ;(setq indent-tabs-mode nil)
	     ;(setq c-basic-offset 4)
             (auto-complete-mode)
	     ;(gtags-mode 1)
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
             (interactive)
             (view-mode)
	     ))

(add-hook 'cscope-minor-mode-hooks
	  '(lambda ()
	     (local-set-key "\M-." 'cscope-find-global-definition)
	     (local-set-key "\M-r" 'cscope-find-this-symbol)
	     (local-set-key "\M-g" 'cscope-find-egrep-pattern)
	     (local-set-key "\C-t" 'cscope-pop-mark)
	     (local-set-key "\M-P" 'cscope-prev-symbol)
	     (local-set-key "\M-N" 'cscope-next-symbol)
	     (local-set-key "\M-s" '(lambda ()
				      (interactive)
				      (switch-to-buffer-other-window "*cscope*")))
	     ))

(add-hook 'cscope-list-entry-hook
	  '(lambda ()
	     (local-set-key "\M-P" 'cscope-prev-symbol)
	     (local-set-key "\M-N" 'cscope-next-symbol)
	     (local-set-key "q" 'delete-window)
	     ))
	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for auto-install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-install)
(add-to-list 'load-path auto-install-directory)
;(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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
;; settings for iswitchb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(iswitchb-mode 1)
(add-hook 'iswitchb-define-mode-map-hook
          'iswitchb-my-keys)

(defun iswitchb-my-keys ()
  "Add my keybindings for iswitchb."
  (define-key iswitchb-mode-map [right] 'iswitchb-next-match)
  (define-key iswitchb-mode-map [left] 'iswitchb-prev-match)
  (define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
  (define-key iswitchb-mode-map " " 'iswitchb-next-match)
  (define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)
  )

(defadvice iswitchb-exhibit
  (after
   iswitchb-exhibit-with-display-buffer
   activate)
  "Show selected buffer in window."
  (when (and (eq iswitchb-method iswitchb-default-method)
             iswitchb-matches)
    (select-window
     (get-buffer-window (cadr (buffer-list))))
    (let ((iswitchb-method 'samewindow))
      (iswitchb-visit-buffer (get-buffer (car iswitchb-matches))))
    (select-window (minibuffer-window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for key-chord
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)
(key-chord-define-global "jk" 'view-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for view-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq view-read-only t)
(require 'viewer)
(viewer-stay-in-setup)
(when window-system
  (setq viewer-modeline-color-unwritable "tomato"
        viewer-modeline-color-view "orange")
  (viewer-change-modeline-color-setup))

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
;; settings for Gauche
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for shell-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'shell-mode-hook
          '(lambda ()
             (ansi-color-for-comint-mode-on)
	     (setq comint-input-ring-file-name "~/.bash_history")
	     (setq comint-input-ring-size 1000000)
	     (comint-read-input-ring t)
             (pcomplete-shell-setup)
             ))

(require 'multi-shell)
(require 'shell-completion)

(setq multi-shell-command "/bin/bash")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun clear-shell ()
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for term-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'multi-term)
;(setq term-unbind-key-list '("C-x" "C-c" "<ESC>"))

(defun my-term-line-mode ()
  (interactive)
  (setq old-term-color
	(face-remap-add-relative 'mode-line :background "dark goldenrod"))
  (term-line-mode))

(defun my-term-char-mode ()
  (interactive)
  (face-remap-remove-relative old-term-color)
  (term-char-mode))
  
(add-hook 'term-mode-hook
	  '(lambda ()
	     (define-key term-raw-map "\C-c\C-j" 'term-line-mode)
	     (define-key term-raw-map "\C-h" 'term-send-backspace)
	     (define-key term-raw-map "\C-y" 'term-paste)
	     (key-chord-define term-raw-map "jk" 'my-term-line-mode)
	     (key-chord-define term-mode-map "jk" 'my-term-char-mode)

	     (set-face-foreground 'term-color-black "black")
	     (set-face-foreground 'term-color-red "red1")
	     (set-face-foreground 'term-color-green "lime green")
	     (set-face-foreground 'term-color-yellow "yellow2")
	     (set-face-foreground 'term-color-blue "DeepSkyBlue3")
	     (set-face-foreground 'term-color-magenta "magenta2")
	     (set-face-foreground 'term-color-cyan "cyan2")
	     (set-face-foreground 'term-color-white "white")
	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'eshell)
(eval-after-load "em-alias"
  '(progn
     (eshell/alias 'll "ls -l")
     (eshell/alias 'la "ls -a")
     ))


(setq eshell-cmpl-ignore-case t)	;補完時に大文字小文字を区別しない
(setq eshell-hist-ignoredups t)		;履歴で重複を無視する
(setq eshell-ask-to-save-history (quote always)) ;確認なしでヒストリ保存

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for gtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evernote-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq evernote-enml-formatter-command
      '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
(require 'evernote-mode)
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for Mew
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'mew)

(setq mew-summary-mode-hook
      '(lambda ()
	 (local-set-key "\C-h" 'mew-summary-prev-page)
	 ))

(setq mew-ssl-verify-level 0
      ;; Does not bring mail automatically at booting time
      mew-auto-get nil
      ;; Leave mail of pop server after get mail
      mew-pop-delete nil
      ;; Add extension ".mew"
      mew-use-suffix t
      ;; Manage unread topic
      mew-use-unread-mark t
      ;; Spam configuration
      mew-spam: "X-Spam-Flag:"
      mew-inbox-action-alist
      '(("X-Spam-Flag:" mew-spam-assassin-or-bsfilter))
      ;; Register password briefly
      mew-use-cached-passwd t
      ;; Does not bring mail at booting time
      mew-auto-get nil
      ;; For Biff
      mew-use-biff t
      mew-use-biff-bell t  ; use alarm
      mew-biff-interval 10 ; minute
      mew-biff-function 'my/biff-function
      mew-summary-form '(type (5 date) " " (25 from) " " t (45 subj))
      )

(defvar mew/gmail-default-alist
  `(("proto"             . "%")
    ("name"              . "Shigeru Yoshida")
    ("user"              . "yshigeru")
    ("imap-user"         . "yshigeru")
    ("mail-domain"       . "gmail.com")
    ("imap-trash-folder" . "%[Gmail]/ゴミ箱")
    ("imap-spam-folder"  . "%[Gmail]/迷惑メール")
    ("fcc"               . "%[Gmail]/送信済みメール")
    ("imap-server"       . "imap.gmail.com")
    ("imap-auth"         . t)
    ("imap-ssl"          . t)
    ("imap-ssl-port"     . "993")
    ("smtp-auth"         . t)
    ("smtp-ssl"          . t)
    ("smtp-ssl-port"     . "465")
    ("smtp-server"       . "smtp.gmail.com")
    ("use-smtp-auth"     . t)))

(defvar mew/gmail-miracle-alist
  `(("proto"             . "%")
    ("name"              . "Shigeru Yoshida")
    ("user"              . "shigeru.yoshida")
    ("imap-user"         . "shigeru.yoshida@miraclelinux.com")
    ("mail-domain"       . "miraclelinux.com")
    ("imap-trash-folder" . "%[Gmail]/ゴミ箱")
    ("imap-spam-folder"  . "%[Gmail]/迷惑メール")
    ("fcc"               . "%[Gmail]/送信済みメール")
    ("imap-server"       . "imap.googlemail.com")
    ("imap-auth"         . t)
    ("imap-ssl"          . t)
    ("imap-ssl-port"     . "993")
    ("smtp-auth"         . t)
    ("smtp-ssl"          . t)
    ("smtp-ssl-port"     . "465")
    ("smtp-server"       . "smtp.googlemail.com")
    ("use-smtp-auth"     . t)))

;; Switch to account by types "C" and renew summary by types "i"
(setq mew-config-alist
      (list `("default" ,@mew/gmail-default-alist)
	    `("miracle" ,@mew/gmail-miracle-alist)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes (quote ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sprit window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
(global-set-key "\C-x@" '(lambda ()
                           (interactive)
                           (split-window-vertically-n 3)))
(global-set-key "\C-x#" '(lambda ()
                           (interactive)
                           (split-window-horizontally-n 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swap buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun swap-screen()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))
(global-set-key [f2] 'swap-screen)
(global-set-key [S-f2] 'swap-screen-with-cursor)
