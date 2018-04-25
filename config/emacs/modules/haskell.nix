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
    ];

    init.hs = dag.entryAfter [ "pkgs" ] ''
      (require 'lsp-ui)
      (require 'lsp-haskell)
      (add-hook 'lsp-mode-hook 'lsp-ui-mode)
      (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
      (add-hook 'haskell-mode-hook 'flycheck-mode)
      ;(require 'company-lsp)
      ;(push 'company-lsp company-backends)
      (setq company-minimum-prefix-length 1)
      (setq company-idle-delay 0)
      (global-company-mode)
      (company-quickhelp-mode)
    '';
  };
}
