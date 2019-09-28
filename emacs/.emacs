(require 'package)
;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; I use use-package to install and configure my packages. My init.el includes the initial setup for package.el and ensures that use-package is installed, since I wanna do that right away.

;; This makes sure that use-package will install the package if it’s not already available. It also means that I should be able to open Emacs for the first time on a fresh Debian box and have my whole environment automatically installed. I’m not totally sure about that, but we’re gettin’ close.

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Always compile packages, and use the newest version available.

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

;; Other stuff

(setq visible-bell 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (powerline solarized-theme helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 40)
(setq helm-autoresize-min-height 0)
(helm-autoresize-mode 1)

(helm-mode 1)

;; I don’t usually use the menu or scroll bar, and they take up useful space.

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

;; There’s a tiny scroll bar that appears in the minibuffer window. This disables that:

(set-window-scroll-bars (minibuffer-window) nil nil)

;; Set default font and configure font resizing

;; I’m partial to Inconsolata.

;; The standard text-scale- functions just resize the text in the current buffer; I’d generally
;; like to resize the text in every buffer, and I usually want to change the size of the modeline,
;; too (this is especially helpful when presenting). These functions and bindings let me resize
;; everything all together!

;; Note that this overrides the default font-related keybindings from sensible-defaults.

(defun cll/open-user-init-file()
  (interactive)
  (find-file user-init-file))

(setq cll/default-font "InconsolataG")
(setq cll/default-font-size 13)
(setq cll/current-font-size cll/default-font-size)

(setq cll/font-change-increment 1.1)
(setq cll/line-spacing 0)

(setq-default line-spacing cll/line-spacing)

(defun cll/font-code ()
  "Return a string representing the current font (like \"Inconsolata-14\")."
  (concat cll/default-font "-" (number-to-string cll/current-font-size)))

(defun cll/set-font-size ()
  "Set the font to `cll/default-font' at `cll/current-font-size'.
Set that for the current frame, and also make it the default for
other, future frames."
  (let ((font-code (cll/font-code)))
    (add-to-list 'default-frame-alist (cons 'font font-code))
    (set-frame-font font-code)))

(defun cll/reset-font-size ()
  "Change font size back to `cll/default-font-size'."
  (interactive)
  (setq cll/current-font-size cll/default-font-size)
  (cll/set-font-size))

(defun cll/increase-font-size ()
  "Increase current font size by a factor of `cll/font-change-increment'."
  (interactive)
  (setq cll/current-font-size
        (ceiling (* cll/current-font-size cll/font-change-increment)))
  (cll/set-font-size))

(defun cll/decrease-font-size ()
  "Decrease current font size by a factor of `cll/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq cll/current-font-size
        (max 1
             (floor (/ cll/current-font-size cll/font-change-increment))))
  (cll/set-font-size))

(define-key global-map (kbd "C-)") 'cll/reset-font-size)
(define-key global-map (kbd "C-+") 'cll/increase-font-size)
(define-key global-map (kbd "C-=") 'cll/increase-font-size)
(define-key global-map (kbd "C-_") 'cll/decrease-font-size)
(define-key global-map (kbd "C--") 'cll/decrease-font-size)

(cll/reset-font-size)


;; Load up a theme

;; Saw this theme on a chat and it is pretty cool, so let's try it.

(use-package nord-theme)
;  :config
;  (load-theme 'solarized-light t)
;
;  (setq solarized-use-variable-pitch nil
;        solarized-height-plus-1 1.0
;        solarized-height-plus-2 1.0
;        solarized-height-plus-3 1.0
;        solarized-height-plus-4 1.0)
;
;  (let ((line (face-attribute 'mode-line :underline)))
;    (set-face-attribute 'mode-line          nil :overline   line)
;    (set-face-attribute 'mode-line-inactive nil :overline   line)
;    (set-face-attribute 'mode-line-inactive nil :underline  line)
;    (set-face-attribute 'mode-line          nil :box        nil)
;    (set-face-attribute 'mode-line-inactive nil :box        nil)
;    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun cll/apply-theme ()
  "Apply the `nord' theme and make frames just slightly transparent."
  (interactive)
  (load-theme 'nord t)
  (transparency 95))

;; If this code is being evaluated by emacs --daemon, ensure that each subsequent frame is themed appropriately.

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (cll/apply-theme))))
  (cll/apply-theme))


(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))
