{ pkgs, nodes, config, lib, ... }:

with lib;

{

  options.mine.profiles.desktop = {
    enable = mkEnableOption "desktop config";
  };

  config = mkIf config.mine.profiles.desktop.enable {

    mine.emacs = {
      enable = true;
      config.usePretest = true;
    };

    mine.userConfig = {
      services.flameshot.enable = true;
    };

    mine.drawing.enable = true;

    mine.taskclient.enable = true;

    mine.x.enable = true;
    mine.wm.enable = true;
    mine.dev.idris.enable = true;
    mine.dev.haskell.enable = true;

    mine.music.client = {
      enable = true;
      server = nodes.yuri;
    };

    nix.extraOptions = ''
      keep-derivations = true
      keep-outputs = true
    '';

    environment.systemPackages = with pkgs; [
      texlive.combined.scheme-full
      nixops
      neofetch
      youtube-dl
      ffmpeg-full
      stack
      imagemagick7Big
      mine.deluge
      nix-prefetch-git
      audacity
      sshfs
      nix-index
      mine.hashsearch
      blockhash
    ];

    services.usbmuxd.enable = true;

    services.dbus.socketActivated = true;

    boot.supportedFilesystems = [ "exfat" "ntfs" ];

    boot.plymouth.enable = true;

    boot.loader.grub.splashImage = config.mine.assets.grub;

  };

}
