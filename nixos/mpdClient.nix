{ config, lib, pkgs, ...}:

let
  password = (import ./private.nix).mpd;
in

{
  imports = [
    ./base.nix
    ./mpd.nix
  ];

  environment.systemPackages = with pkgs; [
    mpc_cli
    sonata
  ];

  environment.variables = {
    MPD_HOST = "${password}@infinisil.io";
    MPD_PORT = "${toString config.mpd.port}";
  };
}
