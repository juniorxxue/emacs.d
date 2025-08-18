;;; init.el --- -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; tweaking for speedup
(setq gc-cons-threshold (* 1024 1024 1024))
(setq gcmh-high-cons-threshold (* 1024 1024 1024))
(setq gcmh-idle-delay-factor 20)
(setq jit-lock-defer-time 0.05)
(setq read-process-output-max (* 1024 1024))
(setq package-native-compile t)

(setq mac-command-modifier 'meta)

(set-frame-font "Iosevka Term 15" nil t)
(setq-default cursor-type 'bar)
(set-cursor-color "#7532a8")

(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(require 'use-package)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/site-lisp/use-package/"))

(defvar my-comment-below-cursor-toggle nil
  "Store whether the region below the cursor is commented or not.
This variable does not persist across Emacs sessions.")

(defun comment-uncomment-below-cursor ()
  "Toggle comment/uncomment from the current position to the end of the buffer."
  (interactive)
  (if (not my-comment-below-cursor-toggle)
      (progn
        ;; If not commented yet, comment the region.
        (comment-region (point) (point-max))
        (setq my-comment-below-cursor-toggle t)
        (message "Commented below cursor."))
    (progn
      ;; If already commented, uncomment the region.
      (uncomment-region (point) (point-max))
      (setq my-comment-below-cursor-toggle nil)
      (message "Uncommented below cursor."))))

;; Bind the toggle to F6
(global-set-key (kbd "<f6>") 'comment-uncomment-below-cursor)


(use-package fontify-face
  :ensure t)

(use-package diminish
  :ensure t)

;; Light theme package (nano)
(use-package nano-theme
  :load-path "site-lisp/nano-theme"
  :defer t) ;; defer actual loading; we'll load via time-based switcher

(use-package nimbus-theme
  :load-path "site-lisp/nimbus-theme"
  :defer t)

;; --- Time-based theme switching (nano 08:00-19:59, nimbus 20:00-07:59) ---
(defun my/apply-theme-based-on-time ()
  "Apply nano (light) between 08:00-19:59 local time; otherwise nimbus (dark)."
  (let* ((hour (string-to-number (format-time-string "%H")))
         (lightp (and (>= hour 8) (< hour 20)))
         (target (if lightp 'nano 'nimbus)))
    (unless (member target custom-enabled-themes)
      (mapc #'disable-theme custom-enabled-themes)
      (ignore-errors (load-theme target t)))))

;; Immediate apply at startup and then every 15 minutes (900s)
(my/apply-theme-based-on-time)
(run-with-timer 0 900 #'my/apply-theme-based-on-time)

;;(use-package nano-modeline
;;  :load-path "site-lisp/nano-modeline"
;;  :config
;;  (nano-modeline nil t))

(use-package auto-save
  :load-path "site-lisp/auto-save"
  :config
  (auto-save-enable)
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving
  )

(use-package smart-hungry-delete
  :ensure t
  :bind (([remap hungry-delete-backward] . smart-hungry-delete-backward-char)
         ([remap hungry-delete-forward] . smart-hungry-delete-forward-char)
         ("M-<delete>" . smart-hungry-delete-forward-char)
         ("M-<backspace>" . smart-hungry-delete-backward-char))
  :init (smart-hungry-delete-add-default-hooks))


(use-package expand-region
  :ensure t
  :bind
  (("M-]" . er/expand-region)
   ("M-[" . er/contract-region))
  )

(use-package reveal-in-osx-finder
  :ensure t)

(use-package better-defaults
  :ensure t
  :config (setq visible-bell nil)
          (setq ring-bell-function 'ignore)
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

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("M-i" . consult-imenu)
         ("C-x b" . consult-buffer))
  :config (consult-customize
           consult-line :inherit-input-method t))

(use-package corfu
  ;; Optional customizations

  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
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
  (corfu-history-mode 1)
  (global-corfu-mode))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  :bind (("M-/" . cape-dabbrev))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
)

;; Use Dabbrev with Corfu!
(use-package dabbrev
;;  :bind (("M-/" . dabbrev-expand))
  :config
  (setq dabbrev-case-fold-search nil)
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` "))

;;(use-package hl-line
;;  :ensure t
;;  :config (global-hl-line-mode 1))

(use-package display-line-numbers
  :config
  (setq display-line-numbers-grow-only t)
  (add-hook 'agda2-mode-hook #'display-line-numbers-mode))

(use-package paren
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t
        show-paren-delay 0.2)
  (add-hook 'agda2-mode-hook #'show-paren-mode))

(defun agda-imenu-create-index ()
  "Create an `imenu` index for Agda files, including Unicode names but excluding module declarations and duplicates."
  (let (index seen-names)
    (goto-char (point-min)) ; Start at the beginning of the buffer
    (while (re-search-forward
            "^\\([[:word:][:multibyte:]'_]+\\)\\s-*:" ; Match type signatures
            nil t)
      (let ((name (match-string 1))) ; Capture the name of the definition
        (unless (member name seen-names) ; Avoid duplicates
          (push name seen-names) ; Track seen names
          (push (cons name (match-beginning 0)) index))))
    (nreverse index))) ; Reverse the list to preserve the order

(defun setup-agda-imenu ()
  "Set up `imenu` for the current buffer in Agda mode."
  ;; Ensure `imenu-create-index-function` is buffer-local
  (setq-local imenu-create-index-function #'agda-imenu-create-index))

(defun agda-imenu-refresh ()
  "Refresh the `imenu` index for the current buffer."
  ;; Ensure `imenu--index-alist` is cleared for a fresh rebuild
  (setq-local imenu--index-alist nil))

;; Hook to set up `imenu` for Agda mode
(add-hook 'agda2-mode-hook
          (lambda ()
            (setup-agda-imenu) ;; Set up `imenu`
            (agda-imenu-refresh))) ;; Refresh the index


;; A few more useful configurations...
(use-package emacs
  :bind
  (("M-v" . yank)
   ("M-c" . kill-ring-save)
   ("M-s" . save-buffer)
   ("M-=" . text-scale-increase)
   ("M--" . text-scale-decrease)
   ("M-z" . undo-only))

  :init
  (setq inhibit-startup-screen t)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  (delete-selection-mode 1)
  )

(use-package ultra-scroll-mac
  :if (eq window-system 'mac)
  :load-path "site-lisp/ultra-scroll-mac" ; if you git clone'd instead of package-vc-install
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mac-mode 1))

(use-package move-text
  :ensure t
  :config (move-text-default-bindings))

(use-package rg
  :ensure t)

(use-package projectile
  :ensure t
  :bind (("M-p" . projectile-find-file))
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  )

(defun treemacs-ignore-specific-files (filename absolute-path)
  "Ignore files ending with '.agdai', '.xmind', or named '.projectile'."
  (or (string-suffix-p ".agdai" filename)
      (string-suffix-p ".xmind" filename)
      (string-equal ".projectile" filename)))

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-no-png-images t)
  (setq treemacs-sorting 'mod-time-desc)

  ;; Add the custom ignore predicate to Treemacs
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-specific-files)
  ;; (setq imenu-auto-rescan t)
  )

;; 1. Define a variable to track the input method during projectile-replace
(defvar-local projectile-replace--active-input-method nil
  "Input method to activate during projectile-replace prompts.")

;; 2. Advice to capture the input method when projectile-replace starts
(advice-add 'projectile-replace :around
  (lambda (orig-fun &rest args)
    (let ((projectile-replace--active-input-method current-input-method))
      (apply orig-fun args))))

;; 3. Advice to activate the input method in EVERY read-string called by projectile-replace
(advice-add 'read-string :around
  (lambda (orig-fun &rest args)
    (if projectile-replace--active-input-method
        (minibuffer-with-setup-hook
            (lambda ()
              (activate-input-method projectile-replace--active-input-method))
          (apply orig-fun args))
      (apply orig-fun args))))

(with-eval-after-load 'treemacs
        (defun treemacs--force-git-update-current-file ()
          (-let [file (treemacs-canonical-path buffer-file-name)]
            (treemacs-run-in-every-buffer
             (when (treemacs-is-path file :in-workspace)
               (treemacs-update-single-file-git-state file)))))
  (add-hook 'after-save-hook 'treemacs--force-git-update-current-file))

(defun now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%D %-I:%M %p")))

(defun today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))


(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((recents   . 10)
                        (projects  . 5)))
  (dashboard-setup-startup-hook)
  )

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
  (setq proof-three-window-mode-policy 'hybrid)
  (global-undo-tree-mode)
  )

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

;; (global-display-line-numbers-mode 1)

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


(define-derived-mode eq-reason-mode text-mode "Equational Reasoning Mode"
  "A custom major mode similar to text-mode with '= {...}' lines treated as comments."
  (setq-local comment-start "= {")
  (font-lock-add-keywords nil '(("^= \\(.*\\)$" 1 font-lock-comment-face)))
)

(provide 'eq-reason-mode)

(use-package tex
  :ensure auctex)

(defmacro defadvice! (symbol arglist &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &rest [WHERE PLACES...] BODY\)"
  (declare (indent defun))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(use-package agda2-mode
  :ensure f
  :bind (
         ("M-l" . agda2-load)
         ("M-j" . agda2-goal-and-context)
         ("M-k" . agda2-goal-and-context-and-inferred)
         ))

(use-package diff-hl
  :ensure f
  :load-path "site-lisp/diff-hl"
  :config
  (global-diff-hl-mode))

(use-package copilot
  :ensure editorconfig
  :ensure f
  :load-path "site-lisp/copilot"
  :init
  (defadvice! +copilot--get-source-a (fn &rest args)
    :around #'copilot--get-source
    (cl-letf (((symbol-function #'warn) #'message))
      (apply fn args)))
  :config
  (add-hook 'agda2-mode-hook 'copilot-mode)
  (add-hook 'coq-mode-hook 'copilot-mode)
  (add-to-list 'copilot-indentation-alist '(agda2-mode 2))
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(defun insert-dashed-line ()
  "Insert a dashed line at the current cursor position."
  (interactive)
  (insert "----------------------------------------"))

(global-set-key (kbd "C--") 'insert-dashed-line)

(defconst agda2-admit-pragmas
  '("{-# OPTIONS --allow-unsolved-metas #-}"
    "{-# OPTIONS --allow-incomplete-matches #-}")
  "The two pragmas to ensure at the top of an Agda file for admitting unsolved metas / incomplete matches.")

(defun agda2--ensure-admit-options-in-buffer ()
  "Ensure both admit OPTIONS pragmas appear in current buffer.
If missing, insert them (first one at top; second after the first if needed).
Returns t if the buffer was modified."
  (save-excursion
    (goto-char (point-min))
    (let ((modified nil)
          (first (car agda2-admit-pragmas))
          (second (cadr agda2-admit-pragmas)))
      ;; If first pragma is missing at top, insert both.
      (unless (looking-at-p (regexp-quote first))
        (insert (concat first "\n" second "\n"))
        (setq modified t))
      ;; If first is present but second is nowhere in the file, insert second after first line.
      (when (and (looking-at-p (regexp-quote first))
                 (not (save-excursion
                        (goto-char (point-min))
                        (search-forward second nil t))))
        ;; Move to end of first line and insert second.
        (goto-char (point-min))
        (forward-line 1)
        (insert (concat second "\n"))
        (setq modified t))
      modified)))

(defun agda2-admit-file ()
  "Insert the admit OPTIONS pragmas at the top of the current Agda file if not already present."
  (interactive)
  (when (buffer-file-name)  ;; only operate on real files
    (save-excursion
      (when (agda2--ensure-admit-options-in-buffer)
      (save-buffer)))))

(defun agda2-admit-directory (dir)
  "Prompt for DIR (defaulting to current buffer's parent directory) and ensure admit OPTIONS
pragmas are in all .agda files directly under it."
  (interactive
   (let* ((file (buffer-file-name))
          (default (if file (file-name-directory file) default-directory))
          (chosen (read-directory-name "Directory with .agda files: " default nil t)))
     (list chosen)))
  (let* ((files (directory-files dir t "\\.agda\\'"))
         (modified-count 0))
    (if (null files)
        (message "No .agda files found in %s" dir)
      (dolist (f files)
        (let ((buf (find-file-noselect f)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (when (agda2--ensure-admit-options-in-buffer)
                (save-buffer)
                (cl-incf modified-count))))))
      (message "Scanned %d .agda files in %s; updated %d of them." (length files) dir modified-count))))