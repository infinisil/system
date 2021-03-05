{ lib, pkgs, config, ... }:
let
  types = lib.types;

  cfg = config.environment.autoUpdate;

  packageType = types.submodule {
    options.attrPath = lib.mkOption {
      type = types.uniq (types.listOf types.str);
    };

    options.url = lib.mkOption {
      type = types.str;
      default = "channel:nixpkgs-unstable";
    };

    options.period = lib.mkOption {
      type = types.str;
      default = "daily";
      description = "See man systemd.time";
    };
  };

in {
  options.environment.autoUpdate.packages = lib.mkOption {
    type = types.attrsOf packageType;
    default = {};
  };

  config = {

    environment.profiles = [ "/var/lib/auto-update/profile" ];

    users.users.auto-update = {
      isSystemUser = true;
    };

    # Necessary for --tarball-ttl
    nix.trustedUsers = [ "auto-update" ];

    systemd.services = lib.mapAttrs' (name: value: lib.nameValuePair "update-${name}" {
      script = ''
        set -x
        out=$(nix-build --tarball-ttl 0 --no-out-link ${lib.escapeShellArg value.url} \
          -A ${lib.escapeShellArg (lib.concatMapStringsSep "." lib.strings.escapeNixString value.attrPath)})
        mkdir -p packages
        flock -s lock ln -sfT "$out" packages/${lib.escapeShellArg name}
        flock -x lock nix-env -p profile -ir packages/*
        flock -x lock nix-env -p profile --delete-generations old
      '';
      path = [ config.nix.package pkgs.utillinux ];
      environment.HOME = "%T/home";
      serviceConfig = {
        Type = "oneshot";
        User = "auto-update";
        PrivateTmp = true;
        StateDirectory = "auto-update";
        WorkingDirectory = "%S/auto-update";
      };
    }) cfg.packages;

    systemd.timers = lib.mapAttrs' (name: value: lib.nameValuePair "update-${name}" {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = value.period;
        Persistent = true;
      };
    }) cfg.packages;

  };

}
