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
            when (value >= 1.0) then \"\"
            when (value >= 0.9) then \"\"
            when (value >= 0.8) then \"\"
            when (value >= 0.7) then \"\"
            when (value >= 0.6) then \"\"
            when (value >= 0.5) then \"\"
            when (value >= 0.4) then \"\"
            when (value >= 0.3) then \"\"
            when (value >= 0.2) then \"\"
            when (value >= 0.1) then \"\"
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
    where path like \"%$(${pkgs.mpc_cli}/bin/mpc current -f '%file%')\"
    "

    ${pkgs.sqlite}/bin/sqlite3 ${musicDir}/beets/beets.db "$sql"
  '';

  mkMpdService = { name, description ? name, script }@attrs: let
    exec = pkgs.writeScript name ''
      #!${pkgs.bash}/bin/bash
      set -euo pipefail
      export PATH=${pkgs.mpc_cli}/bin:${pkgs.beets}/bin:$PATH

      function info() {
        info="$(${query})"
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

