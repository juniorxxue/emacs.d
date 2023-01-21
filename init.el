;;; init.el --- -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(set-frame-font "Jetbrains Mono 16")
(setq-default cursor-type 'bar)

(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(require 'use-package)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/site-lisp/use-package/"))

(use-package diminish
  :ensure t)

(use-package smex
  :ensure t)

(use-package splash-screen
  :load-path "site-lisp/emacs-splash")

(use-package xcode-light-theme
  :load-path "site-lisp/xcode-theme"
  :config (load-theme 'xcode-light t))

(use-package better-defaults
  :ensure t
  :config (setq visible-bell nil)
          (defalias 'yes-or-no-p 'y-or-n-p))

(use-package mood-line
  :ensure t
  :config (mood-line-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(ns mac))
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))

;; (use-package ido-vertical-mode
;;   :ensure t
;;   :config (ido-mode 1)
;;           (ido-vertical-mode 1)
;;           (setq ido-vertical-define-keys 'C-n-and-C-p-only))


(use-package ivy
  :ensure t
  :init (ivy-mode 1) ; globally at startup
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 8)
  (setq ivy-count-format "%d/%d "))

(use-package counsel
  :ensure t
  :bind* ; load when pressed
  (("M-x"     . counsel-M-x)
   ("C-s"     . swiper)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)  ; search for recently edited
   ("C-c g"   . counsel-git)      ; search for files in git repo
   ("C-c j"   . counsel-git-grep) ; search for regexp in git repo
   ("C-c /"   . counsel-ag)       ; Use ag for regexp
   ("C-x l"   . counsel-locate)
   ("<f1> f"  . counsel-describe-function)
   ("<f1> v"  . counsel-describe-variable)
   ("<f1> l"  . counsel-find-library)
   ("<f2> i"  . counsel-info-lookup-symbol)
   ("<f2> u"  . counsel-unicode-char)))     ; Resume last Ivy-based completion

(use-package company
  :ensure t
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package company-statistics
  :ensure t
  :config (add-hook 'after-init-hook 'company-statistics-mode))

(use-package move-text
  :ensure t
  :config (move-text-default-bindings))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Coq Theorem Prover ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package proof-general
  :ensure t
  :config
  (defvar coq-user-tactics-db
  '(("dependent induction" "dep ind" "dependent induction #" t "dependent\\s-+induction")
    ("dependent destruction" "dep des" "dependent destruction #" t "dependent\\s-+destruction")))
  (setq proof-splash-enable nil)
  (setq proof-next-command-insert-space nil)
  (setq proof-three-window-mode-policy 'hybrid))

(use-package proof-script
  :config
  (define-key proof-mode-map (kbd "M-n") 'proof-assert-next-command-interactive)
  (define-key proof-mode-map (kbd "<C-return>") 'proof-goto-point)
  (define-key proof-mode-map (kbd "M-h") 'coq-Check)
  (define-key proof-mode-map (kbd "M-p") 'proof-undo-last-successful-command))

(use-package company-coq
  :ensure t
  :init (setq company-coq-live-on-the-edge t)
  :config (add-hook 'coq-mode-hook #'company-coq-mode)
          (add-hook 'coq-mode-hook (lambda ()
                                     (setq coq-compile-before-require 't))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Agda Theorem Prover ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode))

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(defun string-with-offset (msg)
  (setq offset (/ (- 70 (length msg)) 2))
  (concat
   (make-string offset ?\s)
   msg
   (make-string (- 70 offset) ?\s)))

(defun add-padding (str)
  (concat "--"
          (substring str 2 68)
          "--"))

(defun comment-block-generator ()
  "Generate comment block"
  (interactive)
  (setq comment-message (read-string "Comment: "))
  (insert (concat (make-string 70 ?-)                                (string ?\n)
                  (add-padding (make-string 70 ?\s))                 (string ?\n)
                  (add-padding (string-with-offset comment-message)) (string ?\n)
                  (add-padding (make-string 70 ?\s))                 (string ?\n)
                  (make-string 70 ?-)                                (string ?\n))))
