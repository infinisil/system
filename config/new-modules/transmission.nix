{ config, lib, ... }: {

  options.mine.transmission.enable = lib.mkEnableOption "transmission daemon";

  config = lib.mkIf config.mine.transmission.enable {

    systemd.services.transmission.serviceConfig.BindPaths = [ "/var/lib/torrent/archive" ];

    services.transmission = {
      enable = true;
      home = "/var/lib/torrent";
      settings = {
        download-dir = "/var/lib/torrent/current";
        # script-torrent-done-enabled: Boolean (default = false) Run a script at torrent completion.
        # script-torrent-done-filename =
        rpc-authentication-required = true;
        rpc-password = config.private.passwords."infinisil@torrent.infinisil.com";
        rpc-username = "infinisil";
        rpc-whitelist-enabled = false;
        rpc-host-whitelist-enabled = false;
        download-queue-size = 20;
        seed-queue-size = 20;
        incomplete-dir-enabled = false;
      };
    };

    services.nginx = {
      enable = true;
      virtualHosts."torrent.infinisil.com" = {
        enableACME = true;
        forceSSL = true;
        root = "/webroot";
        basicAuth.infinisil = config.private.passwords."infinisil@torrent.infinisil.com";
        locations."/".proxyPass = "http://localhost:${toString config.services.transmission.settings.rpc-port}";
      };
      virtualHosts."media.infinisil.com" = {
        enableACME = true;
        forceSSL = true;
        basicAuth.infinisil = config.private.passwords."infinisil@media.infinisil.com";
        root = "/var/lib/torrent";
        extraConfig = ''
          autoindex on;
          charset UTF-8;
        '';
      };
    };

    networking.firewall.allowedTCPPorts = [ config.services.transmission.settings.rpc-port ];

    users.users.infinisil.extraGroups = [ "transmission" ];
    users.users.nginx.extraGroups = [ "transmission" ];

  };

}
