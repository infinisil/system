{ pkgs, ... }: {

  imports = [
    ./nixos/modules/profiles/demo.nix
  ];

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = [
  ];

  services.xserver.xkbVariant = "dvp";

}
