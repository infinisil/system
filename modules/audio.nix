{ pkgs, ...}:

{
  imports = [
    ./mpdClient.nix
  ];

  hardware.bluetooth.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    extraConfig = ''
      load-module module-switch-on-connect
    '';
  };

  environment.systemPackages = with pkgs; [
    blueman
    pavucontrol
    cli-visualizer
  ];

  home-manager.users.infinisil = {

    home.file.".config/vis/config".text = ''
      audio.sources=pulse
    '';

  };
}
