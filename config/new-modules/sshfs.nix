{ lib, config, ... }:

with lib;

{

  options.mine.sshMounts = mkOption {
    type = types.attrsOf (types.submodule {
      options = {
        host = mkOption {
          type = types.str;
          description = "Host to connect to, as described in <literal>man sshfs</literal>";
          example = "paul@example.com:/etc";
        };

        identity = mkOption {
          type = types.path;
          description = "Path to identity file";
          example = "/home/paul/.ssh/id_rsa";
        };
      };
    });
    description = "SSHfs auto mountpoints";
    default = {};
  };

  config.fileSystems = mapAttrs' (name: value:
    nameValuePair "/ssh/${name}" {
      device = value.host;
      fsType = "fuse.sshfs";
      options = [
        "x-systemd.automount"
        "_netdev"
        "user"
        "idmap=user"
        "transform_symlinks"
        "IdentityFile=${toString value.identity}"
        "allow_other"
        "default_permissions"
        "uid=1000"
        "gid=100"
      ];
    }
  ) config.mine.sshMounts;
}

