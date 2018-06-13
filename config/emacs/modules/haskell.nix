{ config, lib, epkgs, dag, ... }:

with lib;

{

  options.haskell = mkOption {
    type = types.bool;
    default = true;
    description = "Haskell emacs stuff";
  };

  config = mkIf config.haskell {
    packages = with epkgs; [
      company
      flycheck
      lsp-mode
      lsp-ui
      lsp-haskell
      haskell-mode
      company-quickhelp
      hasky-extensions
    ];

    init.hs = dag.entryAfter [ "pkgs" ] ''
      (require 'lsp-ui)
      (require 'lsp-haskell)
      (add-hook 'lsp-mode-hook 'lsp-ui-mode)
      (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
      (add-hook 'haskell-mode-hook 'flycheck-mode)
      (defun hasky-keys ()
        "Hasky extension key binds"
        (require 'hasky-extensions)
        (local-set-key (kbd "C-c C-y") #'hasky-extensions)
        )
      (add-hook 'haskell-mode-hook 'hasky-keys)
      ;(require 'company-lsp)
      ;(push 'company-lsp company-backends)
      (global-company-mode)
      (company-quickhelp-mode)
    '';
  };
}
