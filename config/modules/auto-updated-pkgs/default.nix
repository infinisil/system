{ lib, pkgs, config, ... }:
let
  types = lib.types;

  cfg = config.environment.autoUpdate;

  jsonFormat = pkgs.formats.json {};

  packageType = types.submodule {

    options.spec = {
      url = lib.mkOption {
        type = types.str;
        description = "Which URL to get nixpkgs from";
        default = "channel:nixpkgs-unstable";
        example = "https://github.com/NixOS/nixpkgs/tarball/master";
      };

      config = lib.mkOption {
        type = types.attrsOf jsonFormat.type;
        description = "Nixpkgs config";
        default = {};
        example = { allowUnfree = true; };
      };

      overlays = lib.mkOption {
        type = types.listOf types.path;
        description = "Nixpkgs overlays";
        default = [];
      };

      attrPath = lib.mkOption {
        type = types.listOf types.str;
        description = "Attribute path";
        example = [ "openssl" ];
      };

      overrides = lib.mkOption {
        type = types.attrsOf jsonFormat.type;
        description = ".override argument";
        default = {};
        example = { enableGui = false; };
      };
    };

    options.cooldown = lib.mkOption {
      type = types.int;
      default = 0;
      description = ''
        The seconds to wait between attempting updates for this package. This
        is essentially the package-specific version of the `interval` option. A
        value of 0 indicates no package-specific duration, only limiting
        frequency of updates by the `interval` option. Underneath, this value is
        passed to the `tarball-ttl` Nix option, see `man nix.conf`
      '';
    };
  };

in {
  options.environment.autoUpdate = {
    enable = lib.mkEnableOption "auto-update";
    profile = lib.mkOption {
      type = types.str;
      readOnly = true;
      default = "/run/auto-update/profile";
    };
    packages = lib.mkOption {
      type = types.attrsOf packageType;
      default = {};
    };
    interval = lib.mkOption {
      type = types.int;
      default = 60 * 60;
      description = "Interval in seconds at which to try to update packages";
    };
    presets.yt-dlp = lib.mkEnableOption "yt-dlp";
  };

  config = lib.mkIf cfg.enable {


    environment.autoUpdate.packages.yt-dlp = lib.mkIf cfg.presets.yt-dlp {
      spec.attrPath = [ "yt-dlp" ];
      spec.overrides.withAlias = true;
    };

    environment.profiles = [ cfg.profile ];

    systemd.services.auto-update = {
      script = ''
        set -euo pipefail
        shopt -s inherit_errexit dotglob

        declare -A packages

        updateSinglePackage() {
          local name=$1
          local cooldown=$2
          local specfile=$3

          echo "Trying to update $name..." >&2
          if latest=$(nix-build \
            --tarball-ttl "$cooldown" \
            --no-out-link \
            ${./nixpkgs.nix} \
            --arg specFile "$specfile"); then
            if [[ -v packages["$name"] ]]; then
              current=''${packages["$name"]}
              if [[ "$current" != "$latest" ]]; then
                echo "Successfully updated $name from $current to $latest" >&2
              else
                echo "No update for $name available" >&2
              fi
            else
              echo "Successfully installed $name as $latest" >&2
            fi
            packages["$name"]="$latest"
          else
            if [[ -v packages["$name"] ]]; then
              echo "Failed to update $name" >&2
            else
              echo "Failed to install $name" >&2
            fi
            unset packages["$name"]
          fi
          echo "" >&2
        }

        updatePackages() {
          echo "Updating packages" >&2
          ${lib.concatStrings (lib.mapAttrsToList (name: value: ''
            updateSinglePackage ${lib.escapeShellArgs [
              name
              (toString value.cooldown)
              (jsonFormat.generate "auto-update-spec-${name}.json" value.spec)
            ]}
          '') cfg.packages)}

          if [[ "''${#packages[@]}" -eq 0 ]]; then
            # Equivalent to nix-env --install --remove-all, if that was supported
            nix-env -p "$RUNTIME_DIRECTORY/profile" --uninstall '.*'
          else
            nix-env -p "$RUNTIME_DIRECTORY/profile" --install --remove-all "''${packages[@]}"
          fi
          nix-env -p "$RUNTIME_DIRECTORY/profile" --delete-generations old
        }

        updatePackages

        systemd-notify READY=1

        while true; do
          echo "Sleeping.." >&2
          sleep ${lib.escapeShellArg (toString cfg.interval)}
          echo "Waking up.." >&2
          updatePackages
        done
      '';
      path = [
        config.nix.package
        pkgs.utillinux
        pkgs.gnutar
        pkgs.xz
        pkgs.gzip
      ];
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      environment.HOME = "%T/home";
      serviceConfig = {
        Type = "notify";
        NotifyAccess = "all";
        DynamicUser = true;
        RuntimeDirectory = "auto-update";
      };
    };

  };

}
