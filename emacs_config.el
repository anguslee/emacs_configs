;; .emacs

(setq-default tab-width 4 indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq x-select-enable-clipboard t)
(set-language-environment "UTF-8")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; install packages
(require 'package)
(add-to-list 'package-archives
	         '("melpa" .
			   "https://melpa.org/packages/"))

(package-initialize)
; (package-refresh-contents)
(defvar my-packages
  '(company company-emoji company-c-headers paredit smartparens rainbow-delimiters scala-mode jdee xcscope
    elpy php-mode google-c-style ecb magit lua-mode color-theme-modern nginx-mode company-nginx
    markdown-mode markdown-mode+ markdown-preview-eww autopair dumb-jump function-args ws-butler
    yasnippet-snippets yasnippet markdown-toc markdownfmt json-mode restclient auctex))
(dolist (p my-packages)
  (unless (package-installed-p p)
    (ignore-errors
      (package-install p))))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; cscope:
(setq cscope-do-not-update-database t)
(require 'xcscope)

;; company
(add-hook 'after-init-hook 'global-company-mode)
(require 'company)
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(require 'company-emoji)
(add-hook 'after-init-hook 'company-emoji-init)

(defun darwin-set-emoji-font (frame)
"Adjust the font settings of FRAME so Emacs NS/Cocoa can display emoji properly."
  (if (and window-system
           (eq system-type 'darwin))
      (ignore-errors
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend))))
;; For when emacs is started with Emacs.app
(darwin-set-emoji-font nil)
;; Hook for when a cocoa frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions 'darwin-set-emoji-font)

;; Custom theme
(cond (window-system
       (load-theme 'deep-blue t)
       (enable-theme 'deep-blue)))

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; rainbow-delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

(column-number-mode t)
(setq default-directory "~/")

;; cscope:
(setq cscope-do-not-update-database t)
(require 'xcscope)

;; cedet:
(require 'cedet)
(setq semantic-load-enable-code-helpers t)
(setq semantic-load-turn-useful-things-on t)
(setq semanticdb-project-roots  (list
        (expand-file-name "/") ) )

(setq semanticdb-project-roots  (list
        (expand-file-name "/") ) )
(setq semanticdb-default-save-directory "~/.semantic")
(setq semanticdb-search-system-databases t)
;; (when (require 'ede nil t)
;;   (global-ede-mode t)) 

;; Customize system include paths for semantic:
;; (semantic-add-system-include "/usr/include/" 'c++-mode)
;; (semantic-add-system-include "/usr/include/" 'c-mode)

(require 'mode-local)
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


;; autopair
(require 'autopair)
(add-hook 'c-mode-common-hook
          #'(lambda () (autopair-mode)))

(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

;; switch-file
(require 'switch-file)
(global-set-key [(control x) (t)] 'switch-cc-to-h)

;; dumb-jump
(dumb-jump-mode)
(setq dumb-jump-default-project "~/Documents/Workspace")
(setq dumb-jump-prefer-searcher 'ag)

;; doxygen
(add-to-list 'load-path
             "~/.emacs.d/site-lisp/doxygen-el")
(require 'doxygen)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; member-functions
(require 'member-functions)
(setq  mf--header-file-extension "hpp")
(setq  mf--source-file-extension "cpp")

;; function-args
(require 'function-args)
(fa-config-default)

;; cc-mode
(require 'cc-mode)
(setq auto-mode-alist
	  (append '(("\\.h$" . c++-mode)
                ("\\.hpp$" . c++-mode)) auto-mode-alist))

(add-hook 'c-mode-hook 'semantic-default-c-setup)
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "linux")
            (c-set-offset 'arglist-cont-nonempty 'c-lineup-arglist)))
(add-hook 'c++-mode-hook 'semantic-default-c-setup)
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "stroustrup")
            (c-set-offset 'arglist-cont-nonempty 'c-lineup-arglist)))

(defun cc-mode-arglist-switch-indent ()
  (interactive)
  (when (eq (caar (c-guess-basic-syntax))
            'arglist-cont-nonempty)
    (let ((old-lineup-func
           (alist-get 'arglist-cont-nonempty
                      c-offsets-alist)))
      (setq c-offsets-alist
            (assq-delete-all 'arglist-cont-nonempty
                             c-offsets-alist))
      (if (eq old-lineup-func
              '+)
          (add-to-list 'c-offsets-alist
                       (cons 'arglist-cont-nonempty 'c-lineup-arglist))
        (add-to-list 'c-offsets-alist
                     (cons 'arglist-cont-nonempty '+))) 
      (c-indent-line-or-region))))

(define-key c-mode-base-map [(meta \`)]
  'cc-mode-arglist-switch-indent)

(global-set-key [(f5)] 'gdb)
(global-set-key [(f7)] 'compile)


;; php-mode
(require 'php-mode)

;; python
(elpy-enable)
;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; google-c-style
(require 'google-c-style)
;(add-hook 'c-mode-common-hook 'google-set-c-style)
;(add-hook 'c-mode-common-hook 'google-make-newline-indent)
;(add-hook 'c++-mode-common-hook 'google-set-c-style)
;(add-hook 'c++-mode-common-hook 'google-make-newline-indent)

(require 'ecb)

(setq ;ecb-use-speedbar-instead-native-tree-buffer 'source
 ecb-windows-width 0.25
 ecb-tip-of-the-day nil)

;; ecb Hot-key binding:
(if (eq system-type 'darwin)
  (progn
    (global-set-key [(control f9)] 'ecb-activate)
    (global-set-key [(control f12)] 'ecb-deactivate))
  (progn 
    (global-set-key [(f9)] 'ecb-activate)
    (global-set-key [(f12)] 'ecb-deactivate)))

;; global key bindings
(global-set-key [(control c) (d) (f)] 'ediff-buffers)
(global-set-key [(control c) (u) (r)] 'uncomment-region)
(global-set-key [(control c) (c) (r)] 'comment-or-uncomment-region)
(global-set-key [(meta p)] 'previous-buffer)
(global-set-key [(meta n)] 'next-buffer)


;; git
;; (require 'git)
;; (require 'git-blamed)
;; (if (eq system-type 'darwin)
;;     (global-set-key [(control f11)] 'magit-status)
;;   (global-set-key [(f11)] 'magit-status))

;; majit
(require 'magit)

;; lua-mode
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(setq lua-indent-level 4)
;;(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(require 'lua-mode)
(add-hook 'lua-mode-hook 'turn-on-font-lock)
(add-hook 'lua-mode-hook 'auto-complete-mode)

;; restclient
(require 'restclient)

;; json-mode
(require 'json-mode)

;; Dired custom keymappings
(require 'dired)
(define-key dired-mode-map [(control c) (control s)] 'find-name-dired)

;; Tramp
(setq tramp-default-method "ssh")

;; auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t) 
(setq TeX-PDF-mode t)
(add-hook 'TeX-mode-hook 'predictive-mode)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; scala mode
(require 'scala-mode)
(setq scala-indent:step 4)
(define-key scala-mode-map [(return)] 'newline-and-indent)
(add-hook 'scala-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scala-mode-hook 'subword-mode)

;; TAGS
(setq path-to-ctags "/usr/bin/ctags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name))))

;; eim https://github.com/wenbinye/emacs-eim
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-eim")
(autoload 'eim-use-package "eim" "Another emacs input method")
;; Tooltip 暂时还不好用
(setq eim-use-tooltip nil)

(register-input-method
 "eim-wb" "euc-cn" 'eim-use-package
 "五笔" "汉字五笔输入法" "wb.txt")
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "1ASC" :slant normal :weight normal :height 113 :width normal)))))

(ignore-errors
  (load-file "~/.emacs.d/platform-settings.el"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jdee-compile-option-encoding "UTF-8")
 '(jdee-maven-program "/usr/local/apache-maven-3.5.0/bin/mvn")
 '(package-selected-packages
   (quote
    (ws-butler company-nginx nginx-mode yasnippet-snippets xcscope smartparens scala-mode restclient rainbow-delimiters php-mode paredit markdownfmt markdown-toc markdown-preview-eww markdown-mode+ magit lua-mode json-mode jdee google-c-style function-args flycheck-pyflakes elpy ecb dumb-jump company-emoji company-c-headers color-theme-modern autopair auctex))))
