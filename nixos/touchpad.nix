
{ config, pkgs, ... }:
{
  services.xserver = {
    enable = true;

    # from github.com/bernerdschaefer/dotfiles/blob/bs-nixos/nixos/configuration.nix
    multitouch = {
      enable = true;
      invertScroll = true;
      buttonsMap = [1 3 2];
    };
    synaptics = {
      #enable = true; # Only applies when multitouch is disabled
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
  };
}
