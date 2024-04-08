;;; init.el --- -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq mac-command-modifier 'meta)

;; (set-frame-parameter nil 'ns-appearance 'dark)
;; (set-frame-parameter nil 'ns-transparent-titlebar t)
;; (set-frame-parameter nil 'alpha-background 80)
;; (set-frame-parameter (selected-frame) 'alpha '(96 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (96 . 50)))

;; (add-to-list 'default-frame-alist '(alpha-background . 80))

(set-frame-font "Iosevka Term 17" nil t)
;; (set-face-attribute 'default nil :height 60)
(setq-default cursor-type 'bar)
(set-cursor-color "#7532a8") 

(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(require 'use-package)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/site-lisp/use-package/"))

(use-package diminish
  :ensure t)

(use-package splash-screen
  :load-path "site-lisp/emacs-splash")

(use-package xcode-light-theme
  :load-path "site-lisp/xcode-theme"
  :config
  (load-theme 'xcode-light t)
  (setq visible-bell t)
  (setq ring-bell-function 'ignore)
  )


(use-package reveal-in-osx-finder
  :ensure t)
  

(use-package better-defaults
  :ensure t
  :config (setq visible-bell nil)
          (show-paren-mode 0)
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

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer))
  :config (consult-customize
           consult-line :inherit-input-method t)
  )

(defun treemacs-ignore-agdai (filename absolute-path)
  (string-suffix-p "agdai" filename))

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-no-png-images t)
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-agdai)    
  )

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :ensure t
  :init
  (global-corfu-mode))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (setq dabbrev-case-fold-search nil)
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` "))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package move-text
  :ensure t
  :config (move-text-default-bindings))

(use-package hl-todo
  :ensure t
  :config (setq hl-todo-keyword-faces
                '(("TODO"   . "#FF0000")
                  ("FIXME"  . "#FF0000")
                  ("DEBUG"  . "#A020F0")
                  ("GOTCHA" . "#FF4500")
                  ("STUB"   . "#1E90FF"))))

(use-package consult-todo
  :ensure t)
  


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
                                     (setq coq-compile-before-require 't)))
          (add-hook 'coq-mode-hook
          (lambda ()
            (setq-local prettify-symbols-alist
                        '(("Alpha" . ?Α) ("Beta" . ?Β) ("Gamma" . ?Γ)
                          ("Delta" . ?Δ) ("Epsilon" . ?Ε) ("Zeta" . ?Ζ)
                          ("Eta" . ?Η) ("Theta" . ?Θ) ("Iota" . ?Ι)
                          ("Kappa" . ?Κ) ("Lambda" . ?Λ) ("Mu" . ?Μ)
                          ("Nu" . ?Ν) ("Xi" . ?Ξ) ("Omicron" . ?Ο)
                          ("Pi" . ?Π) ("Rho" . ?Ρ) ("Sigma" . ?Σ)
                          ("Tau" . ?Τ) ("Upsilon" . ?Υ) ("Phi" . ?Φ)
                          ("Chi" . ?Χ) ("Psi" . ?Ψ) ("Omega" . ?Ω)
                          ("alpha" . ?α) ("beta" . ?β) ("gamma" . ?γ)
                          ("delta" . ?δ) ("epsilon" . ?ε) ("zeta" . ?ζ)
                          ("eta" . ?η) ("theta" . ?θ) ("iota" . ?ι)
                          ("kappa" . ?κ) ("lambda" . ?λ) ("mu" . ?μ)
                          ("nu" . ?ν) ("xi" . ?ξ) ("omicron" . ?ο)
                          ("pi" . ?π) ("rho" . ?ρ) ("sigma" . ?σ)
                          ("tau" . ?τ) ("upsilon" . ?υ) ("phi" . ?φ)
                          ("chi" . ?χ) ("psi" . ?ψ) ("omega" . ?ω))
                        ))))

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
  (concat "--+"
          (substring str 3 67)
          "+--"))

(defun comment-block-generator ()
  "Generate comment block"
  (interactive)
  (setq comment-message (read-string "Comment: "))
  (insert (concat (make-string 70 ?-)                                (string ?\n)
;;                  (add-padding (make-string 70 ?\s))                 (string ?\n)
                  (add-padding (string-with-offset comment-message)) (string ?\n)
;;                  (add-padding (make-string 70 ?\s))                 (string ?\n)
                  (make-string 70 ?-)                                (string ?\n))))

;; (use-package agda2-mode
;;       :defer t
;;       :init
;;       (mapc
;;        (lambda (x) (add-to-list 'face-remapping-alist x))
;;        '((agda2-highlight-datatype-face              . font-lock-type-face)
;;          (agda2-highlight-function-face              . font-lock-type-face)
;;          (agda2-highlight-inductive-constructor-face . font-lock-function-name-face)
;;          (agda2-highlight-keyword-face               . font-lock-keyword-face)
;;          (agda2-highlight-module-face                . font-lock-constant-face)
;;          (agda2-highlight-number-face                . nil)
;;          (agda2-highlight-postulate-face             . font-lock-type-face)
;;          (agda2-highlight-primitive-type-face        . font-lock-type-face)
;;          (agda2-highlight-record-face                . font-lock-type-face))))


(define-derived-mode eq-reason-mode text-mode "Equational Reasoning Mode"
  "A custom major mode similar to text-mode with '= {...}' lines treated as comments."
  (setq-local comment-start "= {")
  (font-lock-add-keywords nil '(("^= \\(.*\\)$" 1 font-lock-comment-face)))
)

(provide 'eq-reason-mode)

(use-package tex
  :ensure auctex)
