;;Õ®”√≈‰÷√
(set-language-environment 'Chinese-GB)
(set-keyboard-coding-system 'euc-cn)
(set-clipboard-coding-system 'euc-cn)
(set-terminal-coding-system 'euc-cn)
(set-buffer-file-coding-system 'euc-cn)
(set-selection-coding-system 'euc-cn)
(modify-coding-system-alist 'process "*" 'euc-cn)
(setq default-process-coding-system 
            '(euc-cn . euc-cn))
(setq-default pathname-coding-system 'euc-cn)
(setq visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p)
(display-time)
(column-number-mode t)
(transient-mark-mode t)
(show-paren-mode t)
(tool-bar-mode -1)
(set-clipboard-coding-system 'ctext)
(condition-case err
     (progn
     (require 'xxx) )
   (error
    (message "Can't load xxx-mode %s" (cdr err))))
(setq-default make-backup-files nil)



(setq load-path(cons "D:/erlang_workspace/emacs-22.3-bin-i386/emacs-22.3/extension" load-path))
(setq load-path(cons "D:/erlang_workspace/emacs-22.3-bin-i386/emacs-22.3/extension/color-theme-6.6.0" load-path))
;; Erlang mode
(setq load-path (cons "D:/Program Files/erl5.7.1/lib/tools-2.6.4/emacs" load-path))
(setq erlang-root-dir "D:/Program Files/erl5.7.1")
(setq exec-path (cons "D:/Program Files/erl5.7.1/bin" exec-path))
(require 'erlang-start)

; wb-line-number
(require 'wb-line-number)
(wb-line-number-toggle)

;;color theme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
;; This is my favorite color theme.
(color-theme-deep-blue)

;; Distel
(let ((distel-dir "D:/erlang_workspace/distel/distel-4.03/elisp"))
(unless (member distel-dir load-path)
    (setq load-path (append load-path (list distel-dir)))))
(require 'distel)
(distel-setup)

;; Some Erlang customizations
(add-hook 'erlang-mode-hook
(lambda ()
;; when starting an Erlang shell in Emacs, default in the node name
    (setq inferior-erlang-machine-options '("-sname" "emacs"))
    ;; add Erlang functions to an imenu menu
    (imenu-add-to-menubar "imenu")))
;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
'(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
"Additional keys to bind when in Erlang shell.")
(add-hook 'erlang-shell-mode-hook
   (lambda ()
     ;; add some Distel bindings to the Erlang shell
     (dolist (spec distel-shell-keys)
       (define-key erlang-shell-mode-map (car spec) (cadr spec)))))