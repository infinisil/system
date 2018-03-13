{ nodes, lib, pkgs, ... }: {

  imports = [
    ../hardware/pc
  ];

  mine.profiles.desktop.enable = true;

  mine.shinas = true;

  mine.rpf.client = {
    server = nodes.yuri;
    webport = 8022;
    sshport = 2022;
    subdomain = "pc";
  };

  environment.systemPackages = with pkgs; [
    steam
  ];

  nix.nixPath = [
    "nixpkgs=${lib.cleanSource <nixpkgs>}"
  ];

  networking = {
    hostName = "paul";
    hostId = "56236562";
  };
}
