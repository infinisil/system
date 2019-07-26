{ options, config, lib, pkgs, ... }:

with lib;

{

  options.mine.profiles.server.enable = mkEnableOption "server profile";

  config = mkIf config.mine.profiles.server.enable {

    mine.radicale.enable = true;

    services.taskserver.enable = true;

    mine.paste.enable = true;

    mine.publicDir.enable = true;

    mine.web = {
      enable = true;
      root = "/webroot/www";
      keys.enable = true;
    };

    mine.gitHost.enable = true;

    mine.dns.enable = true;

    mine.znc.enable = true;

    mine.mail.enable = true;

    boot.loader.timeout = 60;
    boot.loader.grub.splashImage = null;

    environment.systemPackages = with pkgs; [
      youtube-dl
      mine.imgurdl
    ];

    networking.firewall.logRefusedConnections = false;

    services.openssh = {
      enable = true;
      passwordAuthentication = false;
      kexAlgorithms = options.services.openssh.kexAlgorithms.default ++ [
        "diffie-hellman-group14-sha1"
      ];
      macs = options.services.openssh.macs.default ++ [
        "hmac-sha1"
      ];
    };

    services.vnstat.enable = true;

    home-manager.users.infinisil = {
      # https://github.com/keybase/keybase-issues/issues/1712#issuecomment-141226705
      home.sessionVariables.GPG_TTY = "$(tty)";
    };

  };

}
