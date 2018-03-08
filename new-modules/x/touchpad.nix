{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.mine.touchpad;

in

{

  options.mine.touchpad = {
    enable = mkEnableOption "touchpad config";

    driver = mkOption {
      type = types.enum [ "multitouch" "synaptics" "libinput" ];
      default = "multitouch";
      description = "Which driver to use";
    };
  };

  config.services.xserver = mkIf cfg.enable {

    multitouch = {
      enable = cfg.driver == "multitouch";
      invertScroll = true;
      buttonsMap = [1 3 2];
      ignorePalm = true;
    };

    synaptics = {
      enable = cfg.driver == "synaptics";
      #buttonsMap = [ 1 3 2 ];
      tapButtons = true;
      twoFingerScroll = true;
      horizTwoFingerScroll = true;
      scrollDelta = 10;
      minSpeed = "0.7";
      maxSpeed = "1.7";
      palmDetect = true;
      additionalOptions = ''
        Option "FingerHigh" "50"
        Option "FingerLow" "30"
        Option "TapAndDragGesture" "off"
        Option "TapButton1" "1"
        Option "TapButton2" "3"
        Option "TapButton3" "2"
        Option "VertScrollDelta" "-500"
        Option "HorizScrollDelta" "-500"
      '';
    };

    libinput = {
      enable = cfg.driver == "libinput";
      accelSpeed = "1.0";
      naturalScrolling = true;
    };

  };
}
