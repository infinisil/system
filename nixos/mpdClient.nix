{ config, lib, pkgs, ...}:

let
  password = (import ./private.nix).mpd;
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
    MPD_HOST = "${password}@infinisil.io";
    MPD_PORT = "${toString config.mpd.port}";
  };
}
