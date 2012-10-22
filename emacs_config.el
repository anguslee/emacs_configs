;; .emacs


(setq-default tab-width 4 indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq x-select-enable-clipboard t)

(set-language-environment "UTF-8")

;; character encoding auto-detection:
(load-file "~/.emacs.d/site-lisp/unicad.el")
(require 'unicad)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)

;; (pc-selection-mode t)
(column-number-mode t)
(setq default-directory "~/Documents/qunar/workspace/qunar-dev/search-team")

;; font
;;(set-default-font "-adobe-courier-medium-r-normal--18-180-75-75-m-110-iso8859-1")
;;(set-default-font "-adobe-courier-medium-r-normal--14-140-75-75-m-90-iso10646-1")
;; (set-default-font monospace)

;; color themes:
(add-to-list 'load-path' "~/.emacs.d/site-lisp")
(load-file "~/.emacs.d/site-lisp/color-theme.el")
(require 'color-theme)
(cond (window-system
       (color-theme-deep-blue)
))


;; cscope:
(setq cscope-do-not-update-database t)
(add-to-list 'load-path "~/.emacs.d/site-lisp/xcscope")
(load-file "~/.emacs.d/site-lisp/xcscope/xcscope.el")
(require 'xcscope)

;; cedet:
(add-to-list 'load-path "~/.emacs.d/site-lisp/cedet-1.1/common")
;(setq semantic-load-turn-everything-on t)
(setq semantic-load-enable-code-helpers t)
(load-file "~/.emacs.d/site-lisp/cedet-1.1/common/cedet.el")
(global-semantic-highlight-func-mode 1)
(semantic-load-enable-gaudy-code-helpers)
(global-semantic-idle-local-symbol-highlight-mode 1)
(setq semantic-load-turn-useful-things-on t)
(setq semanticdb-project-roots  (list
        (expand-file-name "/") ) )

(setq semanticdb-project-roots  (list
        (expand-file-name "/") ) )
(setq semanticdb-default-save-directory "~/.semantic")
(setq semanticdb-search-system-databases t)
(when (require 'ede nil t)
  (global-ede-mode t)) 

;; Customize system include paths for semantic:
;; (semantic-add-system-include "/usr/include/" 'c++-mode)
;; (semantic-add-system-include "/usr/include/" 'c-mode)

;; malabar
(add-to-list 'load-path "~/.emacs.d/site-lisp/cedet-1.1/common")
(require 'cedet)
(add-to-list 'load-path "~/.emacs.d/site-lisp/malabar-1.5-SNAPSHOT/lisp")
 (semantic-load-enable-minimum-features) ;; or enable more if you wish
(require 'malabar-mode)
(setq malabar-groovy-lib-dir "/home/angus/.emacs.d/site-lisp/malabar-1.5-SNAPSHOT/lib")
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . malabar-mode))
(add-hook 'malabar-mode-hook
           (lambda () 
             (add-hook 'after-save-hook 'malabar-compile-file-silently
                       nil t)))
(add-hook 'malabar-mode-hook (function cscope:hook))
(add-hook 'malabar-mode-hook 'auto-complete-mode)

;; cc-mode
(require 'cc-mode)
;; Create my personal c style which follows the coding standard of the pyramid project
(defconst pyramid-c-style
'((c-basic-offset . 4)
(c-comment-only-line-offset . 0)
(c-hanging-braces-alist . ((substatement-open before after)
(brace-list-open)))
(c-offsets-alist
(statement-block-intro . +)
(substatement-open . 0)
(inline-open . 0)
(substatement-label . 0)
;;(access-label . 0)
(statement-cont . +))
)
"Pyramid C/C++ Programming Style\nThis style is a modification of
stroustrup style. ")
(c-add-style "pyramid" pyramid-c-style)
(setq c-default-style '((c++-mode . "pyramid") (c-mode . "pyramid")
(java-mode . "java") (awk-mode . "awk") (other . "gnu")))

(setq auto-mode-alist
	  (append '(("\\.h$" . c++-mode)) auto-mode-alist))

(add-hook 'c-mode-hook 'semantic-default-c-setup)
(add-hook 'c++-mode-hook 'semantic-default-c-setup)

(define-key c-mode-base-map [(return)] 'newline-and-indent)
(global-set-key [(f5)] 'gdb)
(global-set-key [(f7)] 'compile)
(define-key c-mode-base-map [(control x) (t)] 'eassist-switch-h-cpp) ;override toggle-source()
(define-key c-mode-base-map [(control c) (u)] 'uncomment-region)
(define-key c-mode-base-map [(meta \`)] 'c-indent-command)
;; (define-key c-mode-base-map [(tab)] 'my-indent-or-complete)
;; (define-key c-mode-base-map [(meta p)] 'my-indent-or-complete)
;; (define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)

(setq-mode-local c-mode
                 semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode
                 semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local java-mode
                 semanticdb-find-default-throttle
                 '(project unloaded system recursive))

(global-set-key [(control return)] 'semantic-ia-complete-symbol)


(autoload 'senator-try-expand-semantic "senator")


;; ecb
(add-to-list 'load-path "~/.emacs.d/site-lisp/ecb-2.40")
(setq stack-trace-on-error t)
(require 'ecb)

(setq ;ecb-use-speedbar-instead-native-tree-buffer 'source
      ecb-windows-width 0.25
      ecb-tip-of-the-day nil
 )
;; ecb Hot-key binding:
(global-set-key [(f9)] 'ecb-activate)
(global-set-key [(f12)] 'ecb-deactivate)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "UTF-8")
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; auto-complete-mode:
(add-to-list 'load-path "/home/angus/.emacs.d/site-lisp/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/home/angus/.emacs.d/site-lisp/ac-dict")
(ac-config-default)
(global-set-key [(control c) (a) (c)] 'auto-complete-mode)

;; shortcut for ediff-buffers
(global-set-key [(control c) (d) (f)] 'ediff-buffers)

;; svn:
(require 'psvn)
(global-set-key [(f8)] 'svn-status)

;; git
(add-to-list 'load-path "~/.emacs.d/site-lisp/git")
(require 'git)
(require 'git-blame)
(global-set-key [(f11)] 'git-status)

;; multi-eshell
(require 'multi-eshell)
;; (global-set-key "\C-c .-e" 'multi-eshell)

;; pg
(require 'pg)

;; lua-mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/lua-mode")
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(setq lua-indent-level 4)
;;(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(require 'lua-mode)
(add-hook 'lua-mode-hook 'turn-on-font-lock)
(add-hook 'lua-mode-hook 'auto-complete-mode)
(define-key lua-mode-map [(control c) (u)] 'uncomment-region)
(define-key lua-mode-map [(control c) (c)] 'comment-region)


;; Dired custom keymappings
(define-key dired-mode-map [(control c) (control s)] 'find-name-dired)

;; Tramp
(setq tramp-default-method "ssh")

;; erc settings for l-tuanirc1
(require 'erc)
(setq erc-default-server "l-tuanirc1.corp.qunar.com")

(global-set-key "\C-cef" (lambda () (interactive)
                             (erc :server "l-tuanirc1.corp.qunar.com" :port "6667" :nick "angus-emacs")))
(setq erc-autojoin-channels-alist '(("ircd.ratbox" "#searchteam" "#tech" "#addev" "#dujia")))

;; slime & lisp mode
(setq inferior-lisp-program "/usr/bin/clisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/slime/")
(require 'slime-autoloads)
(slime-setup)
(add-hook 'lisp-mode-hook 'auto-complete-mode)
(define-key lisp-mode-map [(return)] 'newline-and-indent)

;; emacs-nav
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-nav")
(require 'nav)

;; eim
(add-to-list 'load-path "~/.emacs.d/site-lisp/eim-2.4")
(autoload 'eim-use-package "eim" "Another emacs input method")
(setq eim-use-tooltip nil)

(register-input-method
 "eim-wb" "euc-cn" 'eim-use-package
 "五笔" "汉字五笔输入法" "wb.txt")
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")

;; 用 ; 暂时输入英文
(require 'eim-extra)
(global-set-key ";" 'eim-insert-ascii)

;; restclient
(add-to-list 'load-path "~/.emacs.d/site-lisp/restclient")
(require 'restclient)

;; emacs-jabber
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-jabber-0.8.91")
(require 'jabber-autoloads)

