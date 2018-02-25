; company company-lsp flycheck-haskell ghc lsp-mode lsp-haskell haskell-mode

(with-eval-after-load 'lsp-mode
  (require 'lsp-flycheck))
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp-haskell-enable)
