{ config, pkgs, lib, ... }:

with lib;

let
  musicDir = config.mpd.musicDir;

  query = pkgs.writeScript "query" ''
    #!${pkgs.bash}/bin/bash

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

    ${pkgs.sqlite}/bin/sqlite3 ${musicDir}/beets/beets.db "$sql"
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
      Environment = "MPD_HOST=${config.environment.variables.MPD_HOST}";
      ExecStart = "${exec}";
      Restart = "on-failure";
      RestartSec = 1;
      User = "infinisil";
    };
  };
in {

  systemd.services = {
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
        path="${musicDir}/data/$(mpc current -f '%file%')"
        echo Setting rating $rating to $path
        beet mod -y path:"$path" rating="$rating"
        info
      '';
    };

    nope = mkMpdService {
      name = "nope";
      script = ''
        mpc waitmessage nope
        path="${musicDir}/data/$(mpc current -f '%file%')"
        mpc del 0
        echo Noping $path
        beet mod -y path:"$path" nope=1 &
      '';
    };

    tag = mkMpdService {
      name = "tag";
      script = ''
        tag=$(mpc waitmessage tag)
        path="${musicDir}/data/$(mpc current -f '%file%')"
        echo Modifying $path: $tag
        beet mod -y path:"$path" $tag || true
      '';
    };
  };
}

