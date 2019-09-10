{ config, lib, ... }: {

  options.mine.transmission.enable = lib.mkEnableOption "transmission daemon";

  config = lib.mkIf config.mine.transmission.enable {

    services.transmission = {
      enable = true;
      settings = {
        incomplete-dir-enabled = true;
        # script-torrent-done-enabled: Boolean (default = false) Run a script at torrent completion.
        # script-torrent-done-filename =
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

    users.users = lib.genAttrs config.mine.mainUsers (name: {
      extraGroups = [ "transmission" ];
    });

  };

}
