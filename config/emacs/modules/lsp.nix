{ dag, epkgs, pkgs, lib, config, ... }:

with lib;

{

  options.lsp = mkOption {
    type = types.bool;
    default = false;
    description = "LSP config";
  };

  config = mkIf config.lsp {

    packages = with epkgs; [
      lsp-mode
      lsp-ui
      company-lsp
      flycheck
      markdown-mode
    ];

    init.lsp = ''
      (require 'lsp-mode)
      (require 'markdown-mode)
      (setq lsp-prefer-flymake nil)
      (setq lsp-auto-guess-root t)
      (setq lsp-ui-doc-max-height 10)
      (setq lsp-ui-doc-max-width 80)
      (setq lsp-ui-sideline-ignore-duplicate t)
      (setq lsp-document-sync-method 'full)
    '';

  };
}
