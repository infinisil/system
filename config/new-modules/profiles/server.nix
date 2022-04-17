{ options, config, lib, pkgs, ... }:

with lib;

{

  options.mine.profiles.server.enable = mkEnableOption "server profile";

  config = mkIf config.mine.profiles.server.enable {

    boot.loader.timeout = 60;
    boot.loader.grub.splashImage = null;

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

    # https://github.com/NixOS/nixpkgs/pull/80952
    services.nginx.sslCiphers = "EECDH+AESGCM:EDH+AESGCM:" + options.services.nginx.sslCiphers.default;

    services.vnstat.enable = true;

    home-manager.sharedModules = [{
      # Makes sure glibc isn't pulled in, see
      # https://github.com/nix-community/home-manager/issues/2333
      disabledModules = [ "config/i18n.nix" ];
    }];
    home-manager.users.infinisil = {
      # https://github.com/keybase/keybase-issues/issues/1712#issuecomment-141226705
      home.sessionVariables.GPG_TTY = "$(tty)";
    };

  };

}
