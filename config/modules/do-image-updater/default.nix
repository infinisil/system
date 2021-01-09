{ config, pkgs, lib, ... }:
let
  inherit (lib) types;

  dataDir = "/var/lib/do-image-updater";

  someEnabled = instances != {};

  /*
  File system layout:
  /var/lib/do-image-updater/
    gc.lock
    identity: Unique random identity string used to identify DO images managed by this machine
    locks/
      <hash>-image.lock
    passwords/
      <name>.htpasswd: Temporary password file for nginx
    ids/
      <id>: File containing <id>
    instances/
      <name> -> ../ids/<id>

  Activation:
  - For each instance <name> in the configuration, start a systemd oneshot service doing:
    - Acquire locks/<hash>-image.lock (if multiple instances can share the same image this prevents duplication)
    - If the image already exists in DO (with tags <hash>-image and "$(cat identity)")
      - Get its <id>
      - Wait until it's done (in case a previous run exited before it was done)
    - Otherwise:
      - Generate a password into passwords/<name>.htpasswd
      - Upload the image to DO using tags "<hash>-image", "$(cat identity)"
      - Get its <id>
      - Wait until it's done
      - Remove passwords/<name>.htpasswd
    - Release locks/<hash>-image.lock

    - Acquire gc.lock
    - Write <id> to ids/<id>
    - Link instances/<name> to ../ids/<id>
    - Release gc.lock

  - Have a do-image-updater-gc systemd service run periodically which does:
    - Acquire gc.lock
    - For each instances/<name>:
      - If <name> doesn't exists in the configuration, remove instances/<name>
    - For each ids/<id>
      - If it's not referenced by any instances/<name>, delete the image
    - Release gc.lock
  */

  apiRequest = { method, endpoint, data ? null }: ''
    {
      output=$(mktemp)

      code=$(curl -sS -X ${method} -o "$output" --write-out "%{http_code}" \
        -H "Content-Type: application/json" \
        -H "Authorization: Bearer $(cat ${config.services.do-image-updater.tokenFile})" \
        ${lib.optionalString (data != null) "-d '${builtins.toJSON data}'"} \
        'https://api.digitalocean.com/v2/${endpoint}')

      if [[ "$code" -lt 200 || "$code" -ge 300 ]]; then
        echo 'Error: HTTP status code $code when sending a ${method} request to the DigitalOcean API endpoint ${endpoint}${lib.optionalString (data != null) " with data ${builtins.toJSON data}"}' >&2
        cat "$output" >&2
        rm "$output"
        false
      else
        cat "$output"
        rm "$output"
      fi
    }
  '';

  instances = lib.mapAttrs (name: cfg:
    let
      image = (import (pkgs.path + "/nixos") {
        configuration = cfg.imageConfiguration;
      }).config.system.build.digitalOceanImage;
    in cfg // {
      inherit image;
      imagePath = builtins.unsafeDiscardStringContext (baseNameOf image);
    }
  ) config.services.do-image-updater.instances;

  imageServices = lib.mapAttrs' (name: cfg: lib.nameValuePair "do-image-updater-instance-${name}" {
    wantedBy = [ "multi-user.target" ];
    after = [ "nginx.service" ];
    requires = [ "nginx.service" ];
    path = [ pkgs.utillinux pkgs.apacheHttpd pkgs.curl pkgs.jq pkgs.pwgen ];
    serviceConfig = {
      User = "do-image-updater";
      StateDirectory = baseNameOf dataDir;
      WorkingDirectory = dataDir;
    };
    script = ''
      set -euo pipefail
      shopt -s nocasematch

      if [[ ! -f identity ]]; then
        echo "Generating do-image-updater identity" >&2
        uuidgen > identity
      fi

      mkdir -p locks passwords ids instances

      hashLock=4
      eval "exec $hashLock>locks/${cfg.imagePath}.lock"

      flock "$hashLock"

      if ! result=$(${apiRequest {
        method = "GET";
        endpoint = "images/?tag_name='$(cat identity)'";
      }}); then
        flock -u "$hashLock"
        exit 1
      fi

      match=$(jq --arg region ${cfg.region} --arg imagePath ${cfg.imagePath} <<< "$result" \
        '.images | map( select( (.regions | contains([$region])) and (.tags | contains([$imagePath])) )) | first')

      if id=$(jq -er '.id' <<< "$match"); then
        echo "Image exists already and has id $id" >&2
      else
        echo "Image doesn't exist yet, creating it" >&2

        username=do-image-updater
        password=$(pwgen -s 32 1)
        htpasswd -bc passwords/${name}.htpasswd "$username" "$password"

        echo "Sending image upload request" >&2
        if ! result=$(${apiRequest {
          method = "POST";
          endpoint = "images";
          data = {
            name = name;
            url = "http://'$username':'$password@'${config.networking.public.ipv4}/.do-image-updater/${name}/nixos.qcow2.bz2";
            region = cfg.region;
            tags = [ cfg.imagePath "'$(cat identity)'" ];
            distribution = "NixOS";
          };
        }}); then
          rm passwords/${name}.htpasswd
          flock -u "$hashLock"
          exit 1
        fi

        id=$(jq -r '.image.id' <<< "$result")

        laststatus=
        echo "Waiting until the image is available, id is $id" >&2
        while status=$(jq -r '.image.status' <<< "$result") && [[ "$status" == new || "$status" == pending ]]; do
          if [[ "$laststatus" != "$status" ]]; then
            echo "Status is now $status" >&2
          fi
          laststatus=$status

          sleep 5;

          if ! result=$(${apiRequest {
            method = "GET";
            endpoint = "images/'$id'";
          }}); then
            rm passwords/${name}.htpasswd
            flock -u "$hashLock"
            exit 1
          fi
        done

        if [[ "$status" != available ]]; then
          echo "Error while waiting for image to finish: $result" >&2
          rm passwords/${name}.htpasswd
          flock -u "$hashLock"
          exit 1
        fi

        echo "Image now available" >&2

        rm passwords/${name}.htpasswd

      fi

      flock -u "$hashLock"

      gcLock=5
      eval "exec $gcLock>gc.lock"

      flock "$gcLock"

      echo "$id" > "ids/$id"
      ln -fs "../ids/$id" instances/${name}
      echo "Pointing instances/${name} to $id" >&2

      flock -u "$gcLock"
    '';
  }) instances;

  gcService = {
    wantedBy = [ "multi-user.target" ];
    after = [ "nginx.service" ];
    requires = [ "nginx.service" ];
    path = [ pkgs.utillinux pkgs.apacheHttpd pkgs.curl pkgs.jq pkgs.pwgen ];
    serviceConfig = {
      User = "do-image-updater";
      StateDirectory = baseNameOf dataDir;
      WorkingDirectory = dataDir;
    };
    script = ''
      set -euo pipefail
      shopt -s nullglob

      mkdir -p locks passwords ids instances

      gcLock=5
      eval "exec $gcLock>gc.lock"

      while true; do

        flock "$gcLock"

        for nameFile in instances/*; do
          ${lib.concatMapStringsSep "\n" (name: ''
            if [[ "$nameFile" == "instances/${name}" ]]; then
              continue
            fi
          '') (lib.attrNames instances)}

          echo "Instance $nameFile isn't used in the current configuration anymore, deleting it" >&2
          rm "$nameFile"
        done

        for idFile in ids/*; do
          id=$(cat "$idFile")

          ${lib.concatMapStringsSep "\n" (name: ''
            if [[ ! -f instances/${name} ]]; then
              # If the file doesn't exist it means that it hasn't been created yet, so
              # we don't know whether it could refer to the current id, so don't gc it
              continue
            fi
            if [[ "$id" == "$(cat instances/${name})" ]]; then
              # If it does exist, and it points to the id we're currently checking, the id can't be gc'd
              continue
            fi
          '') (lib.attrNames instances)}

          echo "Image ID $id not referenced anymore, deleting it" >&2
          result=$(${apiRequest {
            method = "DELETE";
            endpoint = "images/'$id'";
          }}) || true

          rm "$idFile"

        done

        flock -u "$gcLock"

        sleep 60

      done
    '';
  };

  nginxLocations = lib.mapAttrs' (name: cfg: lib.nameValuePair "/.do-image-updater/${name}/" {
    alias = cfg.image + "/";
    extraConfig = ''
      auth_basic secured;
      auth_basic_user_file ${dataDir}/passwords/${name}.htpasswd;
    '';
  }) instances;

in {
  options.services.do-image-updater = {

    tokenFile = lib.mkOption {
      type = types.path;
      description = ''
        Path to a file containing a DigitalOcean Personal Access Token. Generate
        one with https://cloud.digitalocean.com/account/api/tokens/new?i=1b1f87
        Needs write access and is used to upload the image
      '';
    };

    instances = lib.mkOption {
      default = {};
      type = types.attrsOf (types.submodule ({ name, ... }: {

        options.region = lib.mkOption {
          type = types.str;
          description = ''
            DigitalOcean region to upload the image to.
          '';
        };

        options.imageConfiguration = lib.mkOption {
          type = types.path;
          description = ''
            Path to the NixOS configuration.nix file to use for this DigitalOcean image.
          '';
        };

        options.resultFile = lib.mkOption {
          type = types.path;
          default = "${dataDir}/instances/${name}";
          readOnly = true;
          description = ''
            Path to where the active image ID is written. It is updated automatically
            after a new image has been pushed and activated.
          '';
        };

      }));
    };
  };

  config = lib.mkIf someEnabled {
    services.nginx = {
      enable = true;
      virtualHosts.${config.networking.public.ipv4}.locations = nginxLocations;
    };
    systemd.services = imageServices // {
      do-image-updater-gc = gcService;
    };
    users.groups.do-image-updater = {};
    users.users.do-image-updater = {
      group = "do-image-updater";
      extraGroups = [ "do-api" ];
      isSystemUser = true;
    };
  };
}
