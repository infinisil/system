{ pkgs, config, lib, ... }:

with lib;

let

  cfg = config.mine.music.server;

  query = pkgs.writeScript "query" ''
    #!${pkgs.stdenv.shell}

    sql="
    select artist || \" - \" || title || (
      case when value is null then \"\"
      else (
        \" - \" || (
          case
            when (value = 10.0) then \"\"
            when (value = 9.0) then \"\"
            when (value = 8.0) then \"\"
            when (value = 7.0) then \"\"
            when (value = 6.0) then \"\"
            when (value = 5.0) then \"\"
            when (value = 4.0) then \"\"
            when (value = 3.0) then \"\"
            when (value = 2.0) then \"\"
            when (value = 1.0) then \"\"
            else \"\"
          end
        )
      )
      end
    )
    from items
    left join item_attributes
      on items.id = item_attributes.entity_id
      and item_attributes.key = \"rating\"
    where path like \"%$1\"
    "

    ${pkgs.sqlite}/bin/sqlite3 --init /dev/null ${cfg.musicDir}/beets/beets.db "$sql"
  '';

  mkMpdService = { name, description ? name, script }@attrs: let
    exec = pkgs.writeScript name ''
      #!${pkgs.bash}/bin/bash
      set -euo pipefail
      export PATH=${pkgs.mpc_cli}/bin:${pkgs.beets}/bin:$PATH

      function info() {
        current="$(mpc current -f %file%)"
        if [ -z "$current" ]; then
          return 0
        fi
        info="$(${query} "$current")"
        echo $info
        mpc sendmessage info "$info" || true
      }

      while true; do
        ${script}
      done
    '';
  in {
    inherit description;
    after = [ "mpd.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Environment = "MPD_HOST=${config.mine.mpdHost}";
      ExecStart = "${exec}";
      Restart = "on-failure";
      RestartSec = 1;
      User = "music";
    };
  };

in

{

  config = mkIf cfg.enable (mkMerge [
    {
      services.mpd = {
        enable = true;
        user = cfg.user;
        group = cfg.group;
        musicDirectory = "${cfg.musicDir}/data";
        playlistDirectory = "${cfg.musicDir}/playlists";
        network.port = cfg.port;
        network.listenAddress = "0.0.0.0";
        #extraConfig = ''
        #  replaygain "track"
        #'';
      };

      systemd.services = {
        mpd.after = [ "home-infinisil-music.mount" ];

        info = mkMpdService {
          name = "info";
          script = ''
            mpc idle player
            echo Player updated, updating info
            info
          '';
        };

        updateInfo = mkMpdService {
          name = "updateInfo";
          script = ''
            mpc waitmessage updateInfo
            echo Received updateInfo request, updating info
            info
          '';
        };

        rating = mkMpdService {
          name = "rating";
          script = ''
            rating="$(mpc waitmessage rating)"
            path="${cfg.musicDir}/data/$(mpc current -f '%file%')"
            echo Setting rating $rating to $path
            beet mod -y path:"$path" rating="$rating"
            info
          '';
        };

        nope = mkMpdService {
          name = "nope";
          script = ''
            mpc waitmessage nope
            path="${cfg.musicDir}/data/$(mpc current -f '%file%')"
            mpc del 0
            echo Noping $path
            beet mod -y path:"$path" nope=1 &
          '';
        };

        tag = mkMpdService {
          name = "tag";
          script = ''
            tag=$(mpc waitmessage tag)
            path="${cfg.musicDir}/data/$(mpc current -f '%file%')"
            echo Modifying $path: $tag
            beet mod -y path:"$path" $tag || true
          '';
        };
      };

    }
    (mkIf (!cfg.local) {
      networking.firewall.allowedTCPPorts = [ 80 443 cfg.port cfg.mp3Port cfg.opusPort ];

      services.mpd = {
        extraConfig = ''
          audio_output {
            type            "httpd"
            name            "My HTTP Stream, opus"
            encoder         "opus"
            signal          "music"
            complexity      "10"
            port            "${toString cfg.opusPort}"
            bitrate         "128000"
            format          "48000:16:2"
            max_clients     "0"
            always_on       "yes"
          }
          audio_output {
            type            "httpd"
            name            "My HTTP Stream, normal mp3 lame"
            encoder         "lame"
            port            "${toString cfg.mp3Port}"
            bitrate         "192"
            format          "44100:16:2"
            max_clients     "0"
            always_on "yes"
          }
          password "${cfg.password}@read,add,control"
        '';
      };

      services.nginx.enable = true;
      services.nginx.virtualHosts."tune.infinisil.com" = {
        root = "/webroot";
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:${toString cfg.mp3Port}";
        locations."/opus/".proxyPass = "http://localhost:${toString cfg.opusPort}";
        #basicAuth.infinisil = config.private.passwords."tune.infinisil.com";
      };

    })
    (mkIf cfg.local {
      #systemd.services.mpd.ExecPreStart = mkForce null;
      services.mpd = {
        extraConfig = ''
          audio_output {
            type "pulse"
            name "MPD PulseAudio Output"
            server "127.0.0.1"
          }
        '';
      };

      hardware.pulseaudio.extraConfig = ''
        load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
      '';
    })
    ]);

}
