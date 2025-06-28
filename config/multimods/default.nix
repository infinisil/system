{ sources, ... }: {
  imports = [
    ./on-demand-minecraft

    ./on-demand-minecraft-setup.nix
    ./ssh-access.nix
    ./vpn-setup.nix
    ./dns-records.nix
    ./borg.nix
    ./rtcwake.nix
    ./rtcwake-setup.nix
    ./music.nix
    ./music-setup.nix
    ./freescout.nix
  ];

  defaults.configuration.nixpkgs.overlays = [
    (self: super: {
      inherit sources;
    })
  ] ++ import ../overlays;
}
