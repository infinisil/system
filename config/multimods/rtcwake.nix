{ lib, config, ... }:
let
  inherit (lib) types;

  nixusConfig = config;

in {

  options.rtcwake = lib.mkOption {
    default = {};
    description = ''
      This module allows you to configure a machine to be suspended, but have
      it wake up regularly to check whether it should continue being suspended
      by asking a server. This is useful when you're leaving on a
      vacation and you don't want to keep your machine at home running all the
      time, but still have it be accessible in case you need it, albeit with
      some delay.

      The attribute name of this option is the Nixus node name of the machine
      you want to be suspended.

      To use this module, generate an SSH key pair. This key pair will be
      used by the machine to regularly connect to the server to check whether
      suspension should continue:
      ```bash
      ssh-keygen -t ed25519 -f rtcwake
      ```

      Set the `sshKeyPairFiles` option to the path of the generated file (Nixus
      secret handling will take care of handling the private key correctly).
      Note that the public key with the ending `.pub` is also used.

      Set the `serverNode` option to the Nixus node you want the machine to
      use for regular checks. And if you haven't already done that, you'll have
      to set the `ssh.access.$serverNode.hostKey` option, you'll get an error
      if you haven't.

      After deploying, you can activate the suspending on the machine with
      ```bash
      systemctl start rtcwake
      ```

      And you can stop it on the server machine by logging into the `rtcwake`
      user and removing the file named like the Nixus node that is being
      suspended:
      ```bash
      sudo -u rtcwake -i rm myNode
      ```

      If you want to trigger this from another machine, the `ssh.access` Nixus
      option works well for that:
      ```nix
      ssh.access.someOtherNode.keys.someUser = {
        # publicKey = "..."; # If it's not set elsewhere already
        hasAccessTo.serverNode.rtcwake = true;
      }
      ```

      Then it can be triggered from user `someUser` from node `someOtherNode`
      with
      ```
      ssh rtcwake@$serverNode rm $node
      ```
    '';
    type = types.attrsOf (types.submodule ({ config, ... }: {
      options.serverNode = lib.mkOption {
        type = types.str;
        description = ''
          When waking up, which node to query for whether it should continue to
          be suspended.
        '';
      };

      options.host = lib.mkOption {
        type = types.str;
        default = nixusConfig.nodes.${config.serverNode}.configuration.networking.public.ipv4;
        defaultText = "Public ipv4 address of `serverNode`";
        description = ''
          How to connect to the server.
        '';
      };

      options.interval = lib.mkOption {
        type = types.ints.positive;
        example = 60 * 60;
        description = ''
          Interval in seconds at which to wake up to check the server node.
        '';
      };

      options.sshKeyPairFiles = lib.mkOption {
        type = types.path;
        description = ''
          SSH key pair to use to connect to the server. This file will be the
          private key, copied from the deployer machine to the target node,
          while the same file with the `.pub` ending will be sent to the server
          node. Generate such a keypair using `ssh-keygen`.
        '';
      };
    }));
  };

  config = {


    ssh.access = lib.mapAttrs (nodeName: cfg: {
      keys.rtcwake = {
        publicKey = builtins.readFile (cfg.sshKeyPairFiles + ".pub");
        hasAccessTo.${cfg.serverNode}.rtcwake = true;
      };
    }) config.rtcwake;

    nodes = lib.mkMerge (lib.mapAttrsToList (nodeName: cfg: {
      ${cfg.serverNode}.configuration = {
        users.users.rtcwake = {
          isSystemUser = true;
          group = "rtcwake";
          home = "/var/lib/rtcwake";
          createHome = true;
          useDefaultShell = true;
        };
        users.groups.rtcwake = {};
      };

      ${nodeName}.configuration = { config, pkgs, ... }: {

        secrets.files.rtcwake = {
          file = cfg.sshKeyPairFiles;
          user = "root";
        };

        systemd.services.rtcwake = {
          path = [ pkgs.openssh pkgs.util-linux ];
          script = ''
            set -euo pipefail

            file=${lib.escapeShellArg nodeName}
            interval=${toString cfg.interval}

            runServer() {
              ssh \
                -o ControlMaster=no \
                -o ConnectTimeout=10 \
                -i ${lib.escapeShellArg config.secrets.files.rtcwake.file} \
                rtcwake@${lib.escapeShellArg cfg.host} \
                "$@"
            }

            timedSuspend() {
              echo "Suspending for $interval seconds"
              # Give some time to cancel it or look at the logs
              sleep 10
              rtcwake -m mem -s "$interval"
            }

            continueSuspend() {
              if ! runServer true; then
                echo "Can't reach server, waiting for 30 seconds"
                sleep 30
                if ! runServer true; then
                  echo "Can't reach server, assuming file \"$file\" still exists"
                  return
                fi
                echo "Can reach server now"
              fi
              if runServer test -e "$file"; then
                echo "File \"$file\" still exists, continuing suspend"
                return
              else
                echo "File \"$file\" doesn't exist anymore, stopping suspend"
                return 1
              fi
            }

            echo "Creating file \"$file\""
            runServer touch "$file"

            while
              timedSuspend
              continueSuspend
            do true; done
          '';
        };
      };
    }) config.rtcwake);
  };

}
