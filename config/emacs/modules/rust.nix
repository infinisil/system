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
      cargo
    ];

    init.rust = ''
      (add-hook 'rust-mode-hook 'cargo-minor-mode)

      (add-hook 'rust-mode-hook #'flycheck-mode)

      (setq lsp-ui-sideline--show-symbol t)
    '';

  };

}
