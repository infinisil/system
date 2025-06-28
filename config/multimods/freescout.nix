{ sources, ... }: {

  subdomains = [ "freescout" ];

  nodes.protos.configuration = { config, ... }: {
    imports = [
      (import sources.flake-compat {
        src = sources.freescout-nix-flake;
      }).defaultNix.nixosModules.freescout
    ];

    secrets.files.freescout.file = ../../external/private/freescout-app-key;

    services.freescout = {
      enable = true;
      domain = "freescout.infinisil.com";

      settings.APP_KEY._secret = config.secrets.files.freescout.file;

      databaseSetup.enable = true;

      nginx = {
        forceSSL = true;
        enableACME = true;
      };
    };
  };

}
