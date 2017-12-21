{ config, pkgs, lib, ... }:

with lib;

let
  home = config.home-manager.users.infinisil;

  musicDir = "${home.home.homeDirectory}/Music";
in

{

  home-manager.users.infinisil = {

    programs.beets.settings = {
      directory = "${musicDir}/data";
      library = "${musicDir}/beets/beets.db";
      format_item = "$id: $artist - $album - $title - $rating";
      sort_item = "added- artist+ album+ disc+ track+";

      import = {
        move = true;
        resume = true;
        incremental = true;
        log = "${musicDir}/beets/import.log";
      };

      plugins = [
        "bpm"
        "hook"
        "smartplaylist"
        "fromfilename"
        "edit"
        "fetchart"
        "mpdupdate"
        "ftintitle"
        "replaygain"
        "lastgenre"
        "convert"
        "play"
        "types"
      ];

      hook.hooks = [
        #{
        #  event = "import";
        #  command = "beet write";
        #}
      ];

      types = {
        rating = "float";
        likes = "int";
      };

      play = {
        command = let play = pkgs.writeScript "play" ''
          #!${pkgs.bash}/bin/bash
          export PATH="${pkgs.mpc_cli}/bin:$PATH"
          playlist=''${@: -1}

          add() {
            cat $playlist | sed 's/\/home\/infinisil\/Music\/data\///g' | mpc add
          }

          # If the playlist is empty or --args a(ppend) was given, just load the playlist
          if [ -z "$(mpc playlist)" ] || [ "$1" = "a" ]; then
            cat $playlist | mpc add
          else
            mpc clear
            cat $playlist | mpc add
          fi
          mpc play
        ''; in "${play} $args";
        relative_to = "${musicDir}/data";
        warning_threshold = 1;
      };

      lastgenre = {
        count = 5;
        source = "track";
      };

      replaygain = {
        backend = "bs1770gain";
        overwrite = true;
      };

      mpd = {
        host = "infinisil.com";
        password = config.private.passwords.mpd;
      };

      smartplaylist = {
        playlist_dir = "${musicDir}/playlists";
        relative_to = "${musicDir}/data";
        playlists = mapAttrsToList (name: query: {
          name = "${name}.m3u";
          inherit query;
        }) {
          okay = "^nope:1 ^rating:0..5";
          unrated = "^rating:0..10 ^nope:1";
          decentSongs = "^nope:1 rating:6.. length:..10:00 , ^nope:1 now:1 length:..10:00";
          decentMixes = "^nope:1 rating:6.. length:10:00.. , ^nope:1 now:1 length:10:00..";
          now = "now:1";
        };
      };
    };
  };

}
