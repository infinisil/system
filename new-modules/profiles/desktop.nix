{ pkgs, nodes, config, lib, ... }:

with lib;

{

  options.mine.profiles.desktop = {
    enable = mkEnableOption "desktop config";
  };

  config = mkIf config.mine.profiles.desktop.enable {

    mine.emacs.enable = true;

    mine.x.enable = true;
    mine.wm.enable = true;
    mine.dev.rust.enable = true;

    mine.music.client = {
      enable = true;
      server = nodes.yuri;
    };

    environment.systemPackages = with pkgs; [
      texlive.combined.scheme-full
      nixops
      neofetch
      youtube-dl
      ffmpeg-full
      stack
      imagemagick7Big
    ];

    services.usbmuxd.enable = true;

    services.dbus.socketActivated = true;

    boot.supportedFilesystems = [ "exfat" "ntfs" ];

    boot.plymouth.enable = true;

  };

}