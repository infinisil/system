{ lib, pkgs, ... }: {

  nix.nixPath = [
    # Ruin the config so we don't accidentally run
    # nixos-rebuild switch on the host (thanks grahamc!)
    "nixos-config=${pkgs.writeText "throw-configuration.nix" ''
      throw "Hey dummy, you're on your server! Use NixOps!"
    ''}"
    "nixpkgs=/run/current-system/nixpkgs"
  ];

  system.extraSystemBuilderCmds = ''
    ln -sv ${lib.cleanSource pkgs.path} $out/nixpkgs
  '';

}
