{ lib, config, pkgs, ... }:

with lib;

let
  cfg = config.mine.keylayout;
in

{

  options.mine.keylayout = {
    enable = mkEnableOption "my keylayout config";
  };

  config = mkIf cfg.enable {

    # When in the DigitalOcean web console: `setxkbmap -layout us` (on local machine) and type in correct programmer dvorak
    console.useXkbConfig = true;

    services.xserver = {
      autoRepeatDelay = 200;
      autoRepeatInterval = 25;
      layout = "us";
      xkbOptions = "compose:rwin";
    };

    mine.userConfig = {
      systemd.user.services.setxkbmap = {
        Install.WantedBy = lib.mkForce [];
        Unit.After = lib.mkForce [];
        Unit.PartOf = lib.mkForce [];
      };
    };

  };
}
