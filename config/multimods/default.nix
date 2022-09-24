{
  imports = [
    ./on-demand-minecraft

    ./on-demand-minecraft-setup.nix
    ./ssh-access.nix
    ./vpn-setup.nix
    ./dns-records.nix
    ./zrepl.nix
    ./zrepl-setup.nix
    ./rtcwake.nix
    ./rtcwake-setup.nix
  ];

  defaults.configuration.nixpkgs.overlays = import ../overlays;
}
