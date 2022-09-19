{ pkgs, ... }: {
  console.useXkbConfig = true;

  services.xserver = {
    autoRepeatDelay = 200;
    autoRepeatInterval = 25;
    layout = "us";
    xkbVariant = "dvp";
    xkbOptions = "caps:backspace,compose:ralt";
    # Makes caps-as-backspace key repeat work in X
    displayManager.sessionCommands = ''
      ${pkgs.xorg.xmodmap}/bin/xmodmap -e "clear Lock"
    '';
  };

  # For some reason the gtk greeter resets the layout to the default us one, _unless_ the `~layout` indicator is added! The heck?? This took me hours to debug..
  services.xserver.displayManager.lightdm.greeters.gtk.indicators = [ "~host" "~spacer" "~clock" "~spacer" "~layout" "~session" "~power" ];

  # Tapping left ctrl is escape
  services.interception-tools = {
    enable = true;
    udevmonConfig = let
      dualFunctionKeysConfig = builtins.toFile "dual-function-keys.yaml" ''
        TIMING:
          TAP_MILLISEC: 200
          DOUBLE_TAP_MILLISEC: 0

        MAPPINGS:
          - KEY: KEY_LEFTCTRL
            TAP: KEY_ESC
            HOLD: KEY_LEFTCTRL
      '';
    in ''
      - JOB: |
          ${pkgs.interception-tools}/bin/intercept -g $DEVNODE \
            | ${pkgs.interception-tools-plugins.dual-function-keys}/bin/dual-function-keys -c ${dualFunctionKeysConfig} \
            | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE
        DEVICE:
          EVENTS:
            EV_KEY: [KEY_LEFTCTRL]
    '';
  };

  # Don't let home-manager mess with our system-wide keyboard config
  home-manager.sharedModules = [
    {
      home.keyboard = null;
    }
  ];
}
