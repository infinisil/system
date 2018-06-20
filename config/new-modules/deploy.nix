{ pkgs, lib, config, ... }:

with lib;

let

  cfg = config.mine.deployer;


  rebuild = pkgs.writeScriptBin "rb" ''
    #!${pkgs.stdenv.shell}
    set -euo pipefail
    git="${pkgs.git}/bin/git -C ${cfg.directory}"
    if [ "''${1:-a}" = "--update" ]; then
      shift
      $git checkout "${cfg.branch}"
      $git pull --rebase --recurse-submodules --autostash
    fi
    if [ "$(git rev-parse --abbrev-ref HEAD)" = "${cfg.branch}" ]; then
      echo -n "Enter new branch name: "
      read branch
      $git checkout -b "$branch"
    fi
    $git add --all
    $git commit -v

    branch="$($git rev-parse --abbrev-ref HEAD)"
    msg="$($git log --pretty=format:'%h-%f' -n 1)"
    label="$(printf "%s-%.35s" "$branch" "$msg")"

    nixops="sudo ${pkgs.nixops}/bin/nixops"
    args="${optionalString (cfg.nixops.state != null) "-s \"${cfg.nixops.state}\""} -d ${cfg.nixops.deployment}"
    $nixops set-args $args --argstr label "$label"
    $nixops deploy $args $@
  '';

in

{

  options.mine.deployer = {
    enable = mkEnableOption "this machine as one to deploy the nixops network from from.";

    enableNixpkgs = mkEnableOption "pushing the deploy nixpkgs to the remote";

    remote = mkOption {
      type = types.str;
      example = "git@example.com:paul/config";
      description = "Git remote to use for the initial system config cloning";
    };

    nixops.state = mkOption {
      type = types.nullOr types.path;
      default = null;
      apply = v: if v == null then null else toString v;
      example = "\${config.mine.deployer.directory}/private/deployments.nixops";
      description = ''
        Nixops state file to use for deployments, defaults to nixops default.
      '';
    };

    nixops.deployment = mkOption {
      type = types.str;
      example = "paulsmachines";
      description = ''
        Deployment to use
      '';
    };

    branch = mkOption {
      type = types.str;
      default = "master";
      example = "dev";
      description = "Main git branch";
    };

    directory = mkOption {
      type = types.path;
      example = "/etc/nixos";
      description = "Directory to install the system config to for deployment";
    };

    nixpkgs = mkOption {
      type = types.nullOr types.path;
      default = null;
      example = "\${config.mine.deployer.directory}/nixpkgs";
      description = "Nixpkgs to use for deployers, will be set in NIX_PATH";
    };

  };

  config = mkMerge [

    (mkIf cfg.enable {

      environment.systemPackages = [
        rebuild
        pkgs.nixops
      ];

      nix.nixPath = mkIf (cfg.nixpkgs != null) [
        "nixpkgs=${toString cfg.nixpkgs}"
      ];

      systemd.services.rebuildInit = {
        description = "system rebuild initializer";
        after = [ "network-online.target" ];
        wantedBy = [ "multi-user.target" ];

        script = ''
          dir="${cfg.directory}"
          while [ ! -d "$dir" ]; do dir="$(dirname "$dir")"; done
          owner="$(stat -c %U "$dir")"
          git="${pkgs.git}/bin/git -C \"${cfg.directory}\""
          if [ ! -d "${cfg.directory}" ]; then
            $git clone --recurse-submodules --shallow-submodules \
                -b "${cfg.branch}" "${cfg.remote}" "${cfg.directory}"
            newdir="${cfg.directory}"
            while [ "$dir" != "$newdir" ]; do
              chown "$owner" -R "$newdir"
              newdir="$(dirname "$newdir")"
            done
          fi
        '';
      };

    })
    (mkIf (! cfg.enable && cfg.enableNixpkgs) {
      assertions = [{
        assertion = cfg.nixpkgs != null;
        message = "mine.deployer.nixpkgs has the default value null which doesn't work when mine.deployer.enableNixpkgs is enabled";
      }];

      nix.nixPath = [
        "nixpkgs=${cleanSource cfg.nixpkgs}"
      ];
    })
  ];
}
