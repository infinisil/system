{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.services.murmur';

  iniValueTypes = with types; either str (either int bool);

  port = cfg.config.port or 64738;

  user = config.users.users.murmur.name;
  group = config.users.groups.murmur.name;

  dataDir = "/var/lib/murmur";

  configFile = pkgs.writeText "murmur.ini" (generators.toKeyValue {} cfg.config);

in {

  options.services.murmur' = {

    enable = mkEnableOption "Murmur Mumble server";

    config = mkOption {
      type = types.attrsOf iniValueTypes;
      default = {};
      example = literalExample ''
        {
          registerName = "Paul's Server";
          welcometext = "<br />Welcome to this server running <b>Murmur</b>.<br />Enjoy your stay!<br />";
          usersperchannel = 10;
        }
      '';
      description = ''
        Murmur config. Refer to the official documentation at
        <link xlink:href="https://wiki.mumble.info/wiki/Murmur.ini"\> for the
        available options.
      '';
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to open ports in the firewall for mumble to be
        accessible from outside the local host.
      '';
    };

    acmeDomain = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "example.com";
      description = ''
        Which domains ACME certificate to use for the Mumble server. If
        non-null, this will automatically use the generated ACME certificate
        from <option>security.acme</option> (e.g. via
        <option>services.nginx.virtualHosts.&lt;name&gt;.enableACME</option>)
        as the certificate for the Murmur server.
      '';
    };

  };

  config = mkIf cfg.enable (mkMerge [
    {

      services.murmur'.config.database = "${dataDir}/murmur.sqlite";

      users.users.murmur.group = group;
      users.groups.murmur = {};

      systemd.tmpfiles.rules = [
        "d ${dataDir} - ${user} ${group} -"
        "Z ${dataDir} - ${user} ${group} -"
      ];

      systemd.services.murmur = {
        description = "Murmur Mumble server";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" ];

        serviceConfig = {
          StateDirectory = baseNameOf dataDir;
          WorkingDirectory = dataDir;
          User = user;
          Group = group;
          # The normal `murmur` version is so old that it doesn't support
          # reloading of SSL settings
          ExecStart = "${pkgs.murmur}/bin/murmurd -ini ${configFile} -fg";
          # For reloading SSL settings
          ExecReload = "${pkgs.coreutils}/bin/kill -USR1 $MAINPID";
        };
      };

      networking.firewall = mkIf cfg.openFirewall {
        allowedTCPPorts = [ port ];
        allowedUDPPorts = [ port ];
      };

    }

    (mkIf (cfg.acmeDomain != null) (let

      sslPath = "${dataDir}/full.pem";
      updateCert = ''
        cp ${config.security.acme.directory}/${cfg.acmeDomain}/full.pem ${sslPath}
        chmod g+r ${sslPath}
        chgrp ${group} ${sslPath}
      ''; in {

        services.murmur'.config.sslKey = sslPath;

        # Above tmpfiles rules would set the owner to ${group}, use root instead
        systemd.tmpfiles.rules = [
          "z ${sslPath} - root ${group} -"
        ];

        # This is only needed for when the murmur service starts the first time
        # Because the acme postRun script won't be run until the first renew
        systemd.services.murmur = {
          after = [ "acme-${cfg.acmeDomain}.service" ];
          preStart = updateCert;
          serviceConfig.PermissionsStartOnly = true;
        };

        # Update the certificate when it gets renewed
        security.acme.certs.${cfg.acmeDomain}.postRun = ''
          ${updateCert}
          if systemctl is-active murmur; then
            systemctl --no-block reload murmur
          fi
        '';

      }
    ))
  ]);
}
