{ lib, config, nixus, ... }:
let
  inherit (lib) types;

  nixusConfig = config;

  package = import (import ../../sources).on-demand-minecraft;



  versions = {
    "1.16.5" = nixus.pkgs.fetchFromGitHub {
      owner = "infinisil";
      repo = "on-demand-minecraft";
      rev = "8a7af38594e55d0b87813ece5354508aae6ec668";
      sha256 = "sha256-f74h+dVCtd48Gkwz5rt+8aDcRiuBn/HO/JCsudV/H94=";
    };
    "1.17" = nixus.pkgs.fetchFromGitHub {
      owner = "infinisil";
      repo = "on-demand-minecraft";
      rev = "d7cfb1b4166ac5cd9395e2cc11e498596bf6d986";
      sha256 = "sha256-W7tLuwXjQU6Aeyzxmqads/dDh9ZiUzeC5h/C/chHRcI=";
    };
  };

  format = nixus.pkgs.formats.json {};

  dnsRecords = lib.concatMap (cfg:
    let
      publicIps = nixusConfig.nodes.${cfg.node}.configuration.networking.public;
      absoluteDomain = "${cfg.domain}.";
    in [
      (lib.nameValuePair absoluteDomain {
        A = lib.mkIf publicIps.hasIpv4 publicIps.ipv4;
        AAAA = lib.mkIf publicIps.hasIpv6 publicIps.ipv6;
      })
    ] ++ lib.optional (cfg.settings.port != 25565) (lib.nameValuePair "_minecraft._tcp.${cfg.domain}." {
      SRV.port = cfg.settings.port;
      SRV.target = absoluteDomain;
    })
  ) (lib.filter (cfg: cfg.domain != null) (lib.attrValues config.services.on-demand-minecraft.instances));

  /* <node> -> <name> -> cfg */
  nodeInstances = lib.mapAttrs (node: lib.listToAttrs) (lib.groupBy (v: v.value.node) (lib.mapAttrsToList lib.nameValuePair config.services.on-demand-minecraft.instances));

  nodeConfigs = lib.mapAttrs (node: instances: {
    configuration = {
      networking.firewall.allowedTCPPorts =
        let
          ports = lib.mapAttrsToList (name: cfg: cfg.settings.port) instances;
        in assert lib.unique ports == ports; ports;

      services.do-image-updater = {
        # TODO: Allow do-image-updater to use per-instance tokens
        tokenFile =
          let
            tokens = lib.unique (lib.mapAttrsToList (name: cfg:
              cfg.settings.digitalOcean.tokenFile
            ) instances);
          in assert lib.length tokens == 1; lib.head tokens;
        instances = lib.mapAttrs' (name: cfg: lib.nameValuePair "on-demand-minecraft-${name}" {
          region = cfg.settings.digitalOcean.region;
          imageConfiguration = cfg.imageConfiguration;
        }) instances;
      };

      systemd.services = lib.mapAttrs' (name: cfg: lib.nameValuePair "on-demand-minecraft-${name}" {
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          DynamicUser = true;
          WorkingDirectory = "/var/lib/on-demand-minecraft-${name}";
          StateDirectory = "on-demand-minecraft-${name}";
          ExecStart = "${import versions.${cfg.version}}/bin/on-demand-minecraft ${format.generate "on-demand-minecraft-${name}-settings.json" cfg.settings}";
        };
      }) instances;
    };
  }) nodeInstances;

in {

  options.services.on-demand-minecraft = {
    instances = lib.mkOption {
      default = {};
      type = types.attrsOf (types.submodule ({ config, name, ... }: {

        # Can get the public ip from nodes.*.configuration.networking.public.ipv{4,6}
        options.node = lib.mkOption {
          type = types.str;
        };

        options.version = lib.mkOption {
          type = types.str;
          default = "1.17";
        };

        # Like minecraft.infinisil.com
        # Sets up A record (and SRV record if port isn't 25565)
        options.domain = lib.mkOption {
          type = types.nullOr types.str;
          default = null;
        };

        # TODO: Expose this as a separate Nixus node and have a default configuration for that
        options.imageConfiguration = lib.mkOption {
          type = types.path;
          description = ''
            Path to the NixOS configuration.nix file to use for this minecraft server.
          '';
        };

        options.settings = lib.mkOption {
          type = types.submodule {
            freeformType = format.type;

            options.port = lib.mkOption {
              type = types.port;
              default = 25565;
            };

            options.whitelist = lib.mkOption {
              type = types.attrsOf types.str;
              # No default, because it doesn't make sense for nobody to be able to start it
            };

            options.digitalOcean = {
              region = lib.mkOption {
                type = types.str;
                description = "DigitalOcean region to start the server in.";
              };
              size = lib.mkOption {
                type = types.str;
                description = "DigitalOcean droplet size slug.";
              };
              sshKey = lib.mkOption {
                type = types.str;
                description = "DigitalOcean id for the SSH key to allow access from.";
              };
              volume = lib.mkOption {
                type = types.str;
                description = "DigitalOcean volume id to use for /var/lib/minecraft.";
              };
              tokenFile = lib.mkOption {
                type = types.path;
                description = "Path to where the DigitalOcean token is stored.";
              };
              imageFile = lib.mkOption {
                type = types.path;
                description = "Path to a file containing the id of the DigitalOcean image to use.";
              };
            };
          };
          default = {};
          description = ''
            on-demand-minecraft settings.
          '';
        };

        config.settings.digitalOcean.imageFile = nixusConfig.nodes.${config.node}.configuration.services.do-image-updater.instances."on-demand-minecraft-${name}".resultFile;

      }));
    };
  };

  config = {
    nodes = nodeConfigs;
    dns.records = lib.listToAttrs dnsRecords;
  };
}
