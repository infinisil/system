{ dag, epkgs, pkgs, lib, config, ... }:

with lib;

{

  options.rust = mkOption {
    type = types.bool;
    default = true;
    description = "Rust dev stuff";
  };

  config = mkIf config.rust {

    packages = with epkgs; [
      lsp-rust
      cargo
    ];

    init.rust = ''
      (add-hook 'rust-mode-hook 'cargo-minor-mode)

      (with-eval-after-load 'lsp-mode
        (setq lsp-rust-rls-command '("rls"))
        (require 'lsp-rust))

      (add-hook 'rust-mode-hook #'lsp-rust-enable)
      (add-hook 'rust-mode-hook #'flycheck-mode)

      (setq lsp-ui-sideline--show-symbol t)
    '';

  };

}
