{ lib, config, pkgs, ... }:

with lib;

{

  options.mine.gaming.enable = mkEnableOption "games";

  config = mkIf config.mine.gaming.enable {

    programs.steam.enable = true;

    #boot.blacklistedKernelModules = [ "hid_steam" ];
    boot.blacklistedKernelModules = [ "hid_playstation" ];

    # To get dualsense controllers to work:
    # - Connect both controllers using bluetooth before starting steam
    # - Start steam
    # - Start big picture mode
    # - Start the game
    # - Click the home button and open controller options
    # - Reorder the controller ordering to make your current controller the first one, some games only allow the first controller to control the game


    #services.udev.extraRules = ''
    #  KERNEL=="uinput", SUBSYSTEM=="misc", MODE="0666"
    #'';

    #nixpkgs.config.pulseaudio = true;

    environment.systemPackages = with pkgs; [
      minecraft
      mumble
    ];

  };

}
