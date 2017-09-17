{ config, lib, pkgs, ...}:

{
  imports = [
    ./base.nix
    ./mpd.nix
  ];

  environment.systemPackages = [
    pkgs.mpc_cli
  ] ++ lib.optional config.services.xserver.enable pkgs.sonata;

  environment.variables = {
    MPD_HOST = "${config.passwords.mpd}@infinisil.io";
    MPD_PORT = "${toString config.mpd.port}";
  };
}
