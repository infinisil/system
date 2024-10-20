{ config, lib, pkgs, ... }: {

  imports = [
    ./user.nix
  ];

  time.timeZone = "Europe/Zurich";

  mine.webKeyDirectory.directory = ./wkd;

  security.acme.defaults.email = "acme@infinisil.com";
  security.acme.acceptTerms = true;

  mine.userConfig = {
    programs.gpg.enable = true;
    programs.gpg.settings.encrypt-to = "0x3EAC5A9F2DC4D47E";
    programs.gpg.settings.default-key = "0xE8F1E9EAD284E17D";
  };

  networking.extraHosts = ''
    192.168.178.1 fritz.box
  '';

}
