{ options, config, lib, pkgs, ... }:

with lib;

{

  options.mine.profiles.server.enable = mkEnableOption "server profile";

  config = mkIf config.mine.profiles.server.enable {

    #mine.radicale.enable = true;

    services.taskserver.enable = true;

    mine.paste.enable = true;

    mine.publicDir.enable = true;

    mine.music = {
      server = {
        enable = true;
        local = false;
        musicDir = "/home/infinisil/music";
        user = "infinisil";
        group = "users";
        password = config.private.passwords.mpd;
      };
    };

    mine.eth.mlServer = true;

    mine.web = {
      enable = true;
      root = "/webroot/www";
      keys.enable = true;
    };

    mine.openvpn.server.enable = true;

    mine.gitHost.enable = true;

    mine.dns.enable = true;

    mine.znc.enable = true;

    mine.youtubeDl = {
      enable = true;
      user = "infinisil";
      mpdHost = "${config.private.passwords.mpd}@${config.networking.domain}";
    };

    mine.mail.enable = true;

    boot.loader.timeout = 60;
    boot.loader.grub.splashImage = null;

    # minimalization, taken from <nixpkgs/nixos/modules/profiles>
    sound.enable = false;
    boot.kernelParams = [ "panic=1" "boot.panic_on_fail" ];
    systemd.enableEmergencyMode = false;
    fonts.fontconfig.enable = false;
    i18n.supportedLocales = [ (config.i18n.defaultLocale + "/UTF-8") ];

    environment.systemPackages = with pkgs; [
      youtube-dl
      mine.imgurdl
    ];

    networking.firewall.logRefusedConnections = true;

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

    nix = {
      autoOptimiseStore = true;
      gc = {
        automatic = true;
        dates = "daily";
        options = "--delete-older-than 7d";
      };
    };

    home-manager.users.infinisil = {
      # https://github.com/keybase/keybase-issues/issues/1712#issuecomment-141226705
      home.sessionVariables.GPG_TTY = "$(tty)";
    };

  };

}
