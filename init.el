(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; vertical minibuffer
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(straight-use-package 'ivy-rich)
(require 'ivy-rich)
(ivy-rich-mode 1)

;; better-defaults
(straight-use-package
 '(better-defaults :type git :host github :repo "juniorxxue/better-defaults"))
(defalias 'yes-or-no-p 'y-or-n-p)
(require 'better-defaults)

;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; $PATH
(straight-use-package 'exec-path-from-shell)
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; theme
(set-frame-font "Fira Code 15")
(setq-default cursor-type 'bar)

(straight-use-package
 '(xcode-theme :type git :host github :repo "juniorxxue/xcode-theme"))
(require 'xcode-light-theme)
(load-theme 'xcode-light t)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar .t))

(straight-use-package
 '(splash-screen :type git :host github :repo "juniorxxue/emacs-splash"))
(require 'splash-screen)

(straight-use-package 'mood-line)
(require 'mood-line)
(mood-line-mode)

;; scroll
(straight-use-package 'good-scroll)
(require 'good-scroll)
(good-scroll-mode 1)
(global-set-key (kbd "M-j") #'good-scroll-up)
(global-set-key (kbd "M-k") #'good-scroll-down)

;; find-file-in-project
(straight-use-package 'find-file-in-project)
(require 'find-file-in-project)
(global-set-key (kbd "C-x p") #'find-file-in-project)

;; proof-general
(straight-use-package 'proof-general)
(defvar coq-user-tactics-db
  '(
    ("dependent induction" "dep ind" "dependent induction #" t "dependent\\s-+induction")
    ("dependent destruction" "dep des" "dependent destruction #" t "dependent\\s-+destruction")
    ))
(setq proof-splash-enable nil)
(setq proof-next-command-insert-space nil)

;; force layout
;; see @https://proofgeneral.github.io/doc/master/userman/Customizing-Proof-General/
(setq proof-three-window-mode-policy 'hybrid)

(straight-use-package 'company)
;; let completion be global
(add-hook 'after-init-hook 'global-company-mode)

;; completion option for Require Import
(setq company-coq-live-on-the-edge t)

(straight-use-package 'company-coq)
(add-hook 'coq-mode-hook #'company-coq-mode)
(add-hook 'coq-mode-hook (lambda ()
                           (setq coq-compile-before-require 't)
                           ))

;; handy keybindings
(eval-after-load "proof-script"
  '(progn
     (define-key proof-mode-map (kbd "M-n")
         'proof-assert-next-command-interactive)
     (define-key proof-mode-map (kbd "<C-return>")
       'proof-goto-point)
     (define-key proof-mode-map (kbd "M-p")
       'proof-undo-last-successful-command)))

;; advise M-n
(add-hook 'coq-mode-hook (lambda ()
                           (advice-add 'proof-assert-next-command-interactive
                                       :after (lambda (&optional ARG PRED)
                                                (skip-chars-backward " \t\n")))))
