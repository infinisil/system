{ dag, epkgs, pkgs, lib, config, ... }:

with lib;

{

  options.rust = mkOption {
    type = types.bool;
    default = false;
    description = "Rust dev stuff";
  };

  config = mkIf config.rust {

    lsp = true;

    packages = with epkgs; [
      cargo
      rust-mode
      # rustic ; TODO: Try out with rust-analyzer
    ];

    init.rust = ''
      (add-hook 'rust-mode-hook 'cargo-minor-mode)
      (add-hook 'rust-mode-hook 'lsp)
      (add-hook 'rust-mode-hook 'direnv-update-environment)
    '';

  };

}
