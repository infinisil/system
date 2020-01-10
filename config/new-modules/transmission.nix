{ config, lib, ... }: {

  options.mine.transmission.enable = lib.mkEnableOption "transmission daemon";

  config = lib.mkIf config.mine.transmission.enable {

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

      };
    };

    services.nginx = {
      enable = true;
      virtualHosts."torrent.infinisil.com" = {
        enableACME = true;
        forceSSL = true;
        root = "/webroot";
        basicAuth.infinisil = config.private.passwords."infinisil@torrent.infinisil.com";
        locations."/".proxyPass = "http://localhost:${toString config.services.transmission.port}";
      };
    };

    networking.firewall.allowedTCPPorts = [ config.services.transmission.port ];

    users.users = lib.genAttrs config.mine.mainUsers (name: {
      extraGroups = [ "transmission" ];
    });

  };

}
