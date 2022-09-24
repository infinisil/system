{ lib, config, ... }:
let
  inherit (lib) types;

  pruningOption = lib.mkOption {
    default = {};
    description = ''
      Pruning configuration
    '';
    type = types.submodule ({ config, ... }: {
      options.lastN = lib.mkOption {
        type = types.nullOr types.ints.positive;
        description = ''
          Keep a number of latest snapshots
        '';
      };
      options.grid = lib.mkOption {
        type = types.nullOr types.str;
        description = ''
          See <https://zrepl.github.io/configuration/prune.html#policy-grid>
        '';
      };
    });
  };

in {

  options.zrepl = {

    setups = lib.mkOption {
      default = {};
      type = types.attrsOf (types.submodule ({ name, ... }: {

        options = {

          #name = lib.mkOption {
          #  type = types.str;
          #  default = name;
          #};

          mode = lib.mkOption {
            type = types.enum [ "push" "pull" ];
            description = ''
              The mode used for syncing snapshots, see
              <https://zrepl.github.io/configuration/overview.html?highlight=mode#jobs-how-they-work-together>
            '';
          };

          from = {

            node = lib.mkOption {
              type = types.str;
              description = "The node to use as the source";
            };

            ip = lib.mkOption {
              type = types.str;
              description = "The IP(v4/v6) address to use for the TCP transport";
            };

            port = lib.mkOption {
              type = types.nullOr types.port;
              default = null;
              description = ''
                Only for pull mode, the port for the source to listen on and the puller to connect to
              '';
            };

            fileSystems = lib.mkOption {
              type = types.attrsOf types.bool;
              description = ''
                Filter specifying which filesystems to synchronize. See
                <https://zrepl.github.io/configuration/filter_syntax.html#pattern-filter>
              '';
            };

            sendOptions = lib.mkOption {
              type = types.attrsOf types.bool;
              description = ''
                Send options to use. See
                <https://zrepl.github.io/configuration/sendrecvoptions.html#job-send-options>
              '';
            };

            snapshotInterval = lib.mkOption {
              type = types.nullOr types.str;
              description = ''
                Interval at which to take snapshots. See
                <https://zrepl.github.io/configuration/misc.html?highlight=interval#durations-intervals>
              '';
            };

            pruning = pruningOption;

          };

          to = lib.mkOption {
            default = {};
            type = types.attrsOf (types.submodule ({ name, ... }: {

              options = {

                #node = lib.mkOption {
                #  type = types.str;
                #  default = name;
                #  description = "The node to synchronize to, defaults to the attribute name";
                #};

                ip = lib.mkOption {
                  type = types.str;
                  description = "The IP(v4/v6) address to use for the TCP transport";
                };

                port = lib.mkOption {
                  type = types.nullOr types.port;
                  default = null;
                  description = ''
                    Only for push mode, the port for the sink to listen on and the pusher to connect to
                  '';
                };

                rootFileSystem = lib.mkOption {
                  type = types.str;
                  description = ''
                    Where to put the synced filesystems.
                  '';
                };

                syncInterval = lib.mkOption {
                  type = types.str;
                  description = ''
                    Interval at which to sync snapshots. See
                    <https://zrepl.github.io/configuration/misc.html?highlight=interval#durations-intervals>
                  '';
                };

                pruning = pruningOption;

                placeholder.encryption = lib.mkOption {
                  type = types.enum [ "off" "inherit" ];
                  description = ''
                    See
                    <https://zrepl.github.io/configuration/sendrecvoptions.html?highlight=placeholder#placeholders>
                  '';
                };

              };

            }));
          };

        #from =
        /*
        mySetup = {
          from = {
            node = "<nodeName>";
            tcp.ip = "<ip>";
            filesystems = ...;
            send.<option> = <boolean>;

            # Creates a separate snap job and turns the sending side to "manual" snapshotting and configures pruning correctly
            # Period at which to take local snapshots
            snapshotting = ...;
            # How to prune local snapshots
            pruning = ...;
          };
          # Can be empty -> only snapshotting/pruning
          # Can be local machine -> local snap/replicate
          to.<nodeName> = {
            tcp.ip = "<ip>";
            # Where to put the synced datasets, if pull, will be suffixed with node name. Pull and push need to be reversible
            rootFilesystem = ...;
            # How to prune remote snapshots
            pruning = ...;
          };

          # Period at which to send snapshots
          period = ...;

          transport = {
            type = "tcp";
            tcp.port = <port>;
          };

          mode = "push"|"pull";
        };
        */
        };

      }));

    };

  };

  config.nodes = lib.mkMerge (lib.mapAttrsToList (setupName: setupConfig:
    lib.mkMerge [
      {
        ${setupConfig.from.node}.configuration = {
          services.zrepl.enable = true;
          services.zrepl.settings.jobs = [
            {
              type = "snap";
              name = "nixus-${setupName}-from";
              filesystems = setupConfig.from.fileSystems;
              snapshotting = if setupConfig.from.snapshotInterval == null then {
                type = "manual";
              } else {
                type = "periodic";
                prefix = "nixus_zrepl_snap_";
                interval = setupConfig.from.snapshotInterval;
              };
              pruning.keep = [
                {
                  type = "regex";
                  regex = "^nixus_zrepl_sync_.*";
                }
                (lib.mkIf (setupConfig.from.pruning.lastN != null) {
                  type = "last_n";
                  regex = "^nixus_zrepl_snap_.*";
                  count = setupConfig.from.pruning.lastN;
                })
                (lib.mkIf (setupConfig.from.pruning.grid != null) {
                  type = "grid";
                  regex = "^nixus_zrepl_snap_.*";
                  grid = setupConfig.from.pruning.grid;
                })
              ];
            }
          ] ++ {
            push = lib.mapAttrsToList (toNode: toNodeConfig: {
              type = "push";
              name = "nixus-${setupName}-to-${toNode}";
              connect = {
                type = "tcp";
                address =
                  if toNodeConfig.port == null
                  then throw "zrepl.setups.${setupName}.to.${toNode}.port needs to be set"
                  else "${toNodeConfig.ip}:${toString toNodeConfig.port}";
              };
              filesystems = setupConfig.from.fileSystems;
              send = setupConfig.from.sendOptions;
              snapshotting = {
                type = "periodic";
                prefix = "nixus_zrepl_sync_";
                interval = toNodeConfig.syncInterval;
              };
              pruning = {
                keep_sender = [
                  {
                    type = "regex";
                    regex = "^nixus_zrepl_snap_.*";
                  }
                  {
                    type = "not_replicated";
                    keep_snapshot_at_cursor = false;
                  }
                ];
                keep_receiver = [
                  (lib.mkIf (toNodeConfig.pruning.lastN != null) {
                    type = "last_n";
                    regex = "^nixus_zrepl_sync_.*";
                    count = toNodeConfig.pruning.lastN;
                  })
                  (lib.mkIf (toNodeConfig.pruning.grid != null) {
                    type = "grid";
                    regex = "^nixus_zrepl_sync_.*";
                    grid = toNodeConfig.pruning.grid;
                  })
                ];
              };
            }) setupConfig.to;
            pull = lib.mapAttrsToList (toNode: toNodeConfig: {
              type = "source";
              name = "nixus-${setupName}-to-${toNode}";
              serve = {
                type = "tcp";
                listen =
                  if setupConfig.from.port == null
                  then throw "zrepl.setups.${setupName}.from.port needs to be set"
                  else ":${toString setupConfig.from.port}";
                clients.${toNodeConfig.ip} = toNode;
              };
              filesystems = setupConfig.from.fileSystems;
              send = setupConfig.from.sendOptions;
              snapshotting = {
                type = "periodic";
                prefix = "nixus_zrepl_sync_";
                interval = toNodeConfig.syncInterval;
              };
            }) setupConfig.to;
          }.${setupConfig.mode};
        };
      }
      (lib.mapAttrs (toNode: toNodeConfig: {
        configuration = {

          services.zrepl.enable = true;
          services.zrepl.settings.jobs = [
            {
              push = {
                type = "sink";
                name = "nixus-${setupName}-from";
                serve = {
                  type = "tcp";
                  listen =
                    if toNodeConfig.port == null
                    then throw "zrepl.setups.${setupName}.to.${toNode}.port needs to be set"
                    else ":${toString toNodeConfig.port}";
                  clients.${setupConfig.from.ip} = setupConfig.from.node;
                };
                root_fs = toNodeConfig.rootFileSystem;
                recv.placeholder.encryption = toNodeConfig.placeholder.encryption;
              };
              pull = {
                type = "pull";
                name = "nixus-${setupName}-from";
                connect = {
                  type = "tcp";
                  address =
                    if setupConfig.from.port == null
                    then throw "zrepl.setups.${setupName}.from.port needs to be set"
                    else "${setupConfig.from.ip}:${toString setupConfig.from.port}";
                };
                root_fs = "${toNodeConfig.rootFileSystem}/${setupConfig.from.node}";
                recv.placeholder.encryption = toNodeConfig.placeholder.encryption;
                interval = toNodeConfig.syncInterval;
                pruning = {
                  keep_sender = [
                    {
                      type = "regex";
                      regex = "^nixus_zrepl_snap_.*";
                    }
                    # Should be unnecessary, because we pull, meaning when pruning runs it's always replicated, no need to keep sync snapshots around
                    {
                      type = "not_replicated";
                      keep_snapshot_at_cursor = false;
                    }
                  ];
                  keep_receiver = [
                    (lib.mkIf (toNodeConfig.pruning.lastN != null) {
                      type = "last_n";
                      regex = "^nixus_zrepl_sync_.*";
                      count = toNodeConfig.pruning.lastN;
                    })
                    (lib.mkIf (toNodeConfig.pruning.grid != null) {
                      type = "grid";
                      regex = "^nixus_zrepl_sync_.*";
                      grid = toNodeConfig.pruning.grid;
                    })
                  ];
                };
              };
            }.${setupConfig.mode}
          ];
        };
      }) setupConfig.to)
    ]
  ) config.zrepl.setups);

}
