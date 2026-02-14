{ sources, ... }: {
  imports = [
    ./ssh-access.nix
    ./dns-records.nix
    ./borg.nix
    ./music.nix
    ./music-setup.nix
  ];

  defaults.configuration.nixpkgs.overlays = [
    (self: super: {
      inherit sources;
    })
  ];
}
