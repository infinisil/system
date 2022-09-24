# Remove once https://github.com/NixOS/nixpkgs/pull/192667 is merged
{ lib, config, pkgs, ... }: {

  options.programs.direnv.enable = lib.mkOption {
    type = lib.types.bool;
    default = false;
    example = true;
    description = ''
      Whether to enable direnv integration. Takes care of both installation and
      setting up the sourcing of the shell. Additionally enables nix-direnv
      integration. Note that you need to logout and login for this change to apply.
    '';
  };

  config = lib.mkIf config.programs.direnv.enable {

    nixpkgs.overlays = [(self: super: {
      nix-direnv = super.nix-direnv.overrideAttrs (old: {
        postInstall = ''
          install -m500 -D direnvrc $out/share/direnv/lib/nix-direnv.sh
        '';
      });
    })];

    environment.systemPackages = with pkgs; [
      direnv
      nix-direnv
    ];

    environment.pathsToLink = [
      "/share/direnv"
    ];

    environment.sessionVariables.DIRENV_CONFIG = "/run/current-system/sw/share/direnv";

    programs.bash.interactiveShellInit = ''
      eval "$(direnv hook bash)"
    '';

    programs.zsh.interactiveShellInit = ''
      eval "$(direnv hook zsh)"
    '';

    programs.fish.interactiveShellInit = ''
      direnv hook fish | source
    '';

    # nix options for derivations to persist garbage collection
    nix.settings.keep-outputs = true;
    nix.settings.keep-derivations = true;
  };

}
