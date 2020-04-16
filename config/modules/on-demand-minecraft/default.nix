{ config, lib, pkgs, ... }:
let
  cfg = config.services.on-demand-minecraft;
  port = 25565;

  image = (import (pkgs.path + "/nixos") {
    configuration = ./config.nix;
  }).config.system.build.digitalOceanImage;

  imagePath = builtins.unsafeDiscardStringContext (baseNameOf image);

  apiRequest =
    { method
    , endpoint
    , data ? null }: ''
      {
        echo 'Sending a ${method} request to ${endpoint}${lib.optionalString (data != null) " with data ${builtins.toJSON data}"}' >&2
        curl -sS -X ${method} \
          -H "Content-Type: application/json" \
          -H "Authorization: Bearer $(cat ${config.secrets.doauth.file})" \
          ${lib.optionalString (data != null) "-d '${builtins.toJSON data}'"} \
          'https://api.digitalocean.com/v2/${endpoint}'
      }
    '';

in {

  options.services.on-demand-minecraft = {
    enable = lib.mkEnableOption "On-demand minecraft";

    region = lib.mkOption {
      description = "DigitalOcean region";
      default = "fra1";
    };
  };

  config = lib.mkIf cfg.enable {

    systemd.services.image-updater = {
      wantedBy = [ "multi-user.target" ];
      after = [ "nginx.service" ];
      requires = [ "nginx.service" ];
      path = [ pkgs.apacheHttpd pkgs.curl pkgs.jq pkgs.pwgen ];
      startAt = "minutely";
      serviceConfig = {
        Type = "oneshot";
        StateDirectory = "on-demand-minecraft";
      };
      script = ''
        set -euo pipefail
        activeImagePath=$STATE_DIRECTORY/active-image
        pendingImagePath=$STATE_DIRECTORY/pending-image

        createImage() {
          ip=$(curl -fsS http://169.254.169.254/metadata/v1/interfaces/public/0/ipv4/address)
          password=$(pwgen -s 32 1)
          htpasswd -bc "$STATE_DIRECTORY"/image.htpasswd on-demand-minecraft "$password"

          if ! newInfo=$(${apiRequest {
            method = "POST";
            endpoint = "images";
            data = {
              name = "on-demand-minecraft";
              url = "http://on-demand-minecraft:'$password@$ip'/${imagePath}/nixos.qcow2.bz2";
              region = cfg.region;
              distribution = "NixOS";
              description = imagePath;
            };
          }}); then
            echo "Error creating new image: $newInfo" >&2
            exit 1
          fi
          echo "$newInfo" | jq -r '.image.id' > "$pendingImagePath"
        }

        if [ -f "$pendingImagePath" ]; then
          echo "Checking on the pending image" >&2
          pendingImage=$(cat "$pendingImagePath")
          pendingImageInfo=$(${apiRequest {
            method = "GET";
            endpoint = "images/'$pendingImage'";
          }})
          pendingImageDescription=$(echo "$pendingImageInfo" | jq -r '.image.description')
          echo "Pending image description is $pendingImageDescription" >&2

          if [ "$pendingImageDescription" != ${imagePath} ]; then
            echo "This isn't the image we want, deleting it and creating a new image" >&2
            ${apiRequest {
              method = "DELETE";
              endpoint = "images/'$pendingImage'";
            }}
            createImage
          else
            echo "This is the image we want, checking status" >&2
            pendingImageStatus=$(echo "$pendingImageInfo" | jq -r '.image.status')
            if [ "$pendingImageStatus" != new ] && [ -f "$STATE_DIRECTORY"/image.htpasswd ]; then
              echo "Image not new anymore, removing htpasswd file" >&2
              rm "$STATE_DIRECTORY"/image.htpasswd
            fi
            case $pendingImageStatus in
              available)
                echo "Image not pending anymore, activating it" >&2
                if [ -f "$activeImagePath" ]; then
                  activeImage=$(cat "$activeImagePath")
                  mv -v "$pendingImagePath" "$activeImagePath"
                  echo "Removing the previously active image" >&2
                  ${apiRequest {
                    method = "DELETE";
                    endpoint = "images/'$activeImage'";
                  }}
                else
                  mv -v "$pendingImagePath" "$activeImagePath"
                fi
                ;;
              pending|new)
                echo "Image still pending" >&2
                ;;
              deleted)
                echo "Image was deleted, recreating" >&2
                createImage
                ;;
              *)
                echo "Unhandled image status: $pendingImageStatus" >&2
                exit 1
                ;;
            esac
          fi
        elif [ -f "$activeImagePath" ]; then
          echo "No pending image, but an active one, checking on that" >&2
          activeImage=$(cat "$activeImagePath")
          activeImageInfo=$(${apiRequest {
            method = "GET";
            endpoint = "images/'$activeImage'";
          }})
          activeImageDescription=$(echo "$activeImageInfo" | jq -r '.image.description')
          echo "Active image description is $activeImageDescription" >&2
          if [ "$activeImageDescription" != ${imagePath} ]; then
            echo "This isn't the image we want, create a new image" >&2
            createImage
          else
            echo "This is the image we want, doing nothing" >&2
          fi
        else
          echo "No previous image, creating one" >&2
          createImage
        fi
      '';
    };

    services.nginx = {
      enable = true;
      virtualHosts.${(lib.elemAt config.networking.interfaces.eth0.ipv4.addresses 0).address} = {
        forceSSL = false;
        locations."/${imagePath}/" = {
          alias = image + "/";
          extraConfig = ''
            auth_basic secured;
            auth_basic_user_file /var/lib/on-demand-minecraft/image.htpasswd;
          '';
        };
      };
    };

    networking.firewall.allowedTCPPorts = [ port ];

    systemd.services.on-demand-minecraft = {
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.jq ];
      script = let
        odm = import (import ../../sources).on-demand-minecraft;
        doConfig = pkgs.writeText "config.json" (builtins.toJSON {
          region = cfg.region;
          size = "s-2vcpu-4gb";
          sshKey = "25879389";
          volume = "8b787688-52d2-11ea-9e33-0a58ac14d123";
        });
      in ''
        touch whitelist
        jq '{ whitelist : $whitelist , digitalOcean : $config | (.token |= $token) }' --argjson whitelist "$(cat whitelist)" --argjson config "$(cat ${doConfig})" --arg token "$(cat ${config.secrets.doauth.file})" -n > config.json
        ${odm}/bin/on-demand-minecraft config.json
      '';
      serviceConfig = {
        StateDirectory = "on-demand-minecraft";
        WorkingDirectory = "/var/lib/on-demand-minecraft";
      };
    };
  };

}
