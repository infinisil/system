(evil-mode 1)
(add-hook 'neotree-mode-hook
  (lambda ()
    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(require 'better-defaults)

(global-evil-leader-mode)
(require 'evil-leader)
(require 'which-key)
(require 'recentf)
(recentf-mode)
(require 'helm-config)


(evil-leader/set-leader "<SPC>")
(which-key-add-key-based-replacements "SPC b" "buffer")
(evil-leader/set-key "b k" 'kill-buffer)
(evil-leader/set-key "b s" 'buffer-menu)
(evil-leader/set-key "TAB" 'next-buffer)

(which-key-add-key-based-replacements "SPC e" "eval")
(evil-leader/set-key "e r" 'eval-region)
(evil-leader/set-key "e l" (defun eval-line() (interactive) (eval-region (line-beginning-position) (line-end-position))))

(which-key-add-key-based-replacements "SPC f" "file")
(evil-leader/set-key "f c" (defun emacs-config() (interactive) (find-file user-init-file)))
(evil-leader/set-key "f h" (defun habits() (interactive) (find-file "~/habits/main.org")))
(evil-leader/set-key "f f" 'find-file)
(evil-leader/set-key "f r" 'recentf-open-files)
(evil-leader/set-key "f s" 'save-buffer)


(which-key-add-key-based-replacements "SPC w" "window")
(evil-leader/set-key "w o" 'delete-other-windows)
(evil-leader/set-key "w k" 'delete-windows)

;(setq evil-leader/in-all-states t)
(which-key-mode)

;(require 'company-lsp)
;(add-to-list 'company-backends 'company-lsp)
;(require 'company-ghc)
;(add-to-list 'company-backends 'company-ghc)
;(add-to-list 'company-backends 'company-nixos-options)
;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'after-init-hook 'global-company-mode)

(let
    ((homepath (concat (getenv "HOME") "/.nix-profile/bin"))
     (systempath "/run/current-system/sw/bin"))
    (setenv "PATH" (concat homepath ":" systempath ":" (getenv "PATH")))
    (add-to-list 'exec-path systempath)
    (add-to-list 'exec-path homepath)
    )

(setenv "NIX_REMOTE" "daemon")

(load-theme 'gruvbox)

(with-eval-after-load 'lsp-mode
  (require 'lsp-flycheck))
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp-haskell-enable)

