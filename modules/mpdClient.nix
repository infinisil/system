{ nodes, config, lib, pkgs, ...}:

{
  imports = [
    ./mpd.nix
  ];

  environment.systemPackages = [
    pkgs.mpc_cli
    pkgs.ncmpcpp
  ] ++ lib.optional config.services.xserver.enable pkgs.sonata;

  environment.variables = {
    MPD_HOST = "${config.private.passwords.mpd}@${nodes.server.config.networking.domain}";
    MPD_PORT = "${toString config.mpd.port}";
  };
}
