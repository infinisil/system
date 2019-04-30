{ config, pkgs, lib, ... }:
let

  cfg = config.mine.hueadm;
  inherit (lib) types;

  command = "${pkgs.hueadm}/bin/hueadm"
    + lib.optionalString (cfg.host != null) " -H${cfg.host}"
    + lib.optionalString (cfg.user != null) " -U${cfg.user}";

in {

  options.mine.hueadm = {
    enable = lib.mkEnableOption "hueadm config";

    host = lib.mkOption {
      type = types.nullOr types.str;
      description = ''
        IP of the HUE bridge. Use <command>hueadm search</command> to find
        bridges in your network.
      '';
    };

    user = lib.mkOption {
      type = types.nullOr (types.strMatching "[0-9a-zA-Z]{40}");
      description = ''
        Registered Username. Use <command>hueadm register
        <replaceable>NAME</replaceable></command> to register a new user.
      '';
    };

    controls = lib.mkEnableOption "this machine controlling the lights automatically";

  };

  config = lib.mkIf cfg.enable {

    mine.binalias.hueadm = command;

    systemd.services.hue-off = lib.mkIf cfg.controls {
      wantedBy = [ "sleep.target" ];
      before = [ "sleep.target" ];
      environment.HOME = "/var/empty";
      serviceConfig.ExecStart = "${command} group 0 off";
    };

  };
}
