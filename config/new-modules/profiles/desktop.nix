{ pkgs, nodes, config, lib, ... }:

with lib;

{

  options.mine.profiles.desktop = {
    enable = mkEnableOption "desktop config";
  };

  config = mkIf config.mine.profiles.desktop.enable {

    services.printing.enable = true;
    programs.system-config-printer.enable = true;

    mine.userConfig = {
      services.flameshot.enable = true;
      services.gpg-agent = {
        enable = true;
        extraConfig = ''
          pinentry-program ${pkgs.pinentry.qt}/bin/pinentry
        '';
      };
    };

    mine.taskclient.enable = true;

    mine.x.enable = true;
    mine.wm.enable = true;
    #mine.dev.idris.enable = true;
    mine.dev.haskell.enable = true;

    nix.extraOptions = ''
      keep-derivations = true
      keep-outputs = true
    '';

    environment.systemPackages = with pkgs; [
      neofetch
      ffmpeg-full
      imagemagick7Big
      nix-prefetch-git
      sshfs
      nix-index
      mine.soph
      mine.imgurdl
      mine.Dark
      unrar
      cachix
    ];

    services.usbmuxd.enable = true;

    boot.supportedFilesystems = [ "exfat" "ntfs" ];

    boot.loader.grub.splashImage = config.mine.assets.blurred;

  };

}
