{ config, lib, pkgs, ...}:

let

  domain = (import <cfg/hosts>).dobby.networking.domain;

in

{
  imports = [
    ./base.nix
    ./mpd.nix
  ];

  environment.systemPackages = [
    pkgs.mpc_cli
  ] ++ lib.optional config.services.xserver.enable pkgs.sonata;

  environment.variables = {
    MPD_HOST = "${config.passwords.mpd}@${domain}";
    MPD_PORT = "${toString config.mpd.port}";
  };
}
