{ pkgs, ... }:

let

  stable = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "7f6f0c49f0e8d24346bd32c3dec20cc60108a005";
    sha256 = "1k6p0ayv5riqm4bnyxpd1aw9l34dk96qk9vngmd08lr7h8v3s285";
  };

  stablePkgs = import stable {};

in
{

  environment.systemPackages = [
    stablePkgs.criu
  ];

  nixpkgs.overlays = [ (self: super: {
    linux_4_9 = super.linux_4_9.override {
      extraConfig = ''
        EXPERT? y
        EMBEDDED? y
        CHECKPOINT_RESTORE? y
      '';
    };
  })];

}
