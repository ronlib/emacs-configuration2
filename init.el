;;; package --- Summary
;;; Commentary:
;;; Requisites: Emacs >= 24
(require 'package)
;;; code:
(package-initialize)

;; (let ((default-directory  "~/git/emacs-configuration2/elisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(python-mode magit yasnippet autopair find-file-in-repository flycheck helm xcscope helm-cscope pony-mode projectile helm-projectile web-mode zenburn-theme which-key helm-swoop neotree js2-mode highlight-symbol smartparens company company-rtags))

;; No longer used  jedi auto-complete

;; (package-refresh-contents)
;; (mapc 'install-if-needed to-install)

(desktop-save-mode 1)
(setq-default indent-tabs-mode nil)

;; -------------------- Macros --------------------

(defmacro after (mode &rest body)
  `(eval-after-load ,mode
     '(progn ,@body)))

;; -------------------- Initializations --------------------

;; (require 'helm-cscope)
;; (require 'yasnippet)
(require 'auto-complete)
(require 'autopair)
(require 'flycheck)
;; (require 'pony-mode)
(global-flycheck-mode)

(global-set-key [f7] 'find-file-in-repository)

; auto-complete mode extra settings
;; (require 'auto-complete-config)
;; (setq
;;  ac-auto-start 2
;;  ac-override-local-map nil
;;  ac-use-menu-map t
;;  ac-candidate-limit 20)

;; -------------------- auto-complete --------------------

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")

;; -------------------- company --------------------
(require 'company)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'company-mode)

;; -------------------- rtags --------------------
(cmake-ide-setup)
(require 'rtags)
(require 'ac-rtags)
(setq rtags-completion-enabled t)
(require 'company-rtags)
(push 'company-rtags company-backends)
(require 'helm-rtags)
(setq rtags-display-result-backend 'helm)

;; ------------------ cmake-ide ------------------



;; ------------------ smartparens ------------------
;; (require 'smartparens)
;; (require 'smartparens-config)
;; (smartparens-global-mode nil)

;; ------------------ Python ------------------

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'python-mode-hook 'highlight-symbol-mode)
;; (add-hook 'python-mode-hook 'auto-complete-mode)


;; ;; Jedi settings
;; (require 'jedi)
;; It's also required to run "pip install --user jedi" and "pip
;; install --user epc" to get the Python side of the library work
;; correctly.
;; With the same interpreter you're using.

;; if you need to change your python intepreter, if you want to change it
;; (setq jedi:server-command
;;       '("python2" "/home/andrea/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py"))

(add-hook 'python-mode-hook
	  (lambda ()
	    (jedi:setup)
	    (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (set-key (kbd "M-.") 'jedi:goto-definition)))

;; No need for ido togather with helm
;; (require 'ido)
;; (ido-mode t)

;; -------------------- javascript settings --------------------
;; (add-hook 'javascript-mode-hook 'yas-minor-mode)
;; (add-hook 'js-mode-hook 'js2-minor-mode)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)
;; (setq js2-highlight-level 3)

;; -------------------- extra nice things --------------------
;; use shift to move around windows
(windmove-default-keybindings 'shift)
(show-paren-mode t)
 ; Turn beep off
(setq visible-bell nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(define-key key-translation-map (kbd "M-ת") (kbd "M-<"))
(define-key key-translation-map (kbd "M-ץ") (kbd "M->"))

;; -------------------- magit --------------------
(require 'magit)
(global-set-key "\C-xg" 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
;; (global-set-key [f7] 'find-file-in-repository)

;; -------------------- helm --------------------
(require 'helm)
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "M-z")  'helm-select-action) ; list actions using C-z

;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x f") 'helm-projectile-find-file-in-known-projects)
(global-set-key (kbd "M-x") 'helm-M-x)

(helm-mode 1)

;; (projectile-global-mode)
;; (setq projectile-enable-caching t)

;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (setq web-mode-enable-current-element-highlight t)
;; (setq web-mode-enable-current-column-highlight t)

;; (defun my-web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setq web-mode-markup-indent-offset 2)
;; )
;; (add-hook 'web-mode-hook 'my-web-mode-hook)


;; -------------------- look and feel --------------------

(load-theme 'zenburn t)
(setq-default tab-width 2)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-linum-mode t)
;; (set-face-attribute 'default nil :font "DejaVu Sans Mono-12")
;; (set-face-attribute 'default nil :font "Monospace Regular-12")

;; (require 'which-key)
;; (setq which-key-use-C-h-for-paging t
;;       which-key-prevent-C-h-from-cycling t)
;; (which-key-setup-side-window-right)
;; (which-key-mode t)

;; ------------------ helm-swoop ------------------

(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(global-set-key (kbd "M-x") 'helm-M-x)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-horizontally)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; ------------------ ropemacs ------------------
;; (setq ropemacs-enable-shortcuts nil) (setq ropemacs-local-prefix "C-c C-p")
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")

;; ------------------ neotree ------------------
;; (require 'neotree)

(setq dired-listing-switches "-aBhl --group-directories-first")


(provide '.emacs)
;;; .emacs ends here
