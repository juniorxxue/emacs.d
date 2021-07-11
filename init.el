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
(straight-use-package 'selectrum)
(require 'selectrum)
(selectrum-mode +1)

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
(straight-use-package
 '(github-theme :type git :host github :repo "juniorxxue/github-theme"))
(load-theme 'github t)

(straight-use-package 'mood-line)
(require 'mood-line)
(mood-line-mode)

;; proof-general
(straight-use-package 'proof-general)
(setq proof-splash-enable nil)
(setq proof-next-command-insert-space nil)

(straight-use-package 'company)
(setq company-coq-live-on-the-edge t)

(straight-use-package 'company-coq)
(add-hook 'coq-mode-hook #'company-coq-mode)
(add-hook 'coq-mode-hook (lambda ()
                           (setq coq-compile-before-require 't)
                           ))

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
