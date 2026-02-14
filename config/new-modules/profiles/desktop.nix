{ pkgs, nodes, config, lib, ... }:

with lib;

{

  options.mine.profiles.desktop = {
    enable = mkEnableOption "desktop config";
  };

  config = mkIf config.mine.profiles.desktop.enable {

    services.printing.enable = true;
    programs.system-config-printer.enable = true;

    # Needed for pinentry-gnome to work: https://github.com/NixOS/nixpkgs/issues/112914#issuecomment-850310537
    services.gnome.gnome-keyring.enable = true;

    mine.userConfig = {
      services.flameshot.enable = true;
      services.gpg-agent = {
        enable = true;
        extraConfig = ''
          pinentry-program ${pkgs.pinentry-gnome3}/bin/pinentry
        '';
      };
    };

    mine.x.enable = true;

    nix.settings.keep-derivations = true;
    nix.settings.keep-outputs = true;

    environment.systemPackages = with pkgs; [
      neofetch
      ffmpeg-full
      imagemagickBig
      nix-prefetch-git
      sshfs
      mine.soph
      mine.imgurdl
      mine.Dark
      unrar
      cachix
    ];

    boot.supportedFilesystems = [ "exfat" "ntfs" ];

  };

}
