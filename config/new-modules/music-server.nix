{ lib, config, pkgs, ... }:
let
  musicDir = "/var/lib/music";
in {

  options.services.music-server = {
    enable = lib.mkEnableOption "music server";
  };

  config = lib.mkIf config.services.music-server.enable {
    users.users.music = {
      isSystemUser = true;
      group = "music";
      # Allow login
      useDefaultShell = true;
      home = "${musicDir}/home";
      createHome = true;
    };
    users.groups.music = {};

    home-manager.users.music = {

      programs.zsh.enable = true;

      home.packages = with pkgs; [
        mpc_cli
      ];

      home.sessionVariables.MPD_HOST = "${config.private.passwords.mpd}@localhost";
      home.sessionVariables.MPD_PORT = "6600";

      programs.beets.settings = {
        directory = "${musicDir}/data";
        library = "${musicDir}/beets/beets.db";
        format_item = "$id: $artist - $album - $title - $rating";
        sort_item = "added- artist+ album+ disc+ track+";
        acoustid.apikey = "ex5RgecjNm";

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
          "chroma"
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
            #!${pkgs.stdenv.shell}
            export PATH="${pkgs.mpc_cli}/bin:$PATH"
            playlist=''${@: -1}

            add() {
              cat $playlist | sed 's|${musicDir}/data/||g' | mpc add
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
          backend = "ffmpeg";
          overwrite = true;
        };

        mpd = {
          host = "localhost";
          password = config.private.passwords.mpd;
        };

        smartplaylist = let
          col = prefix: attrs:
            lib.flatten (lib.attrValues (lib.mapAttrs (name: value:
              if lib.isAttrs value
              then col "${prefix}${name}." value
              else {
                name = "${prefix}${name}.m3u";
                query = value;
              })
            attrs));
          createPlaylists = col "";
          in {
          playlist_dir = "${musicDir}/playlists";
          relative_to = "${musicDir}/data";
          playlists = createPlaylists {
            ".enka" = "enka:1 ^rating:0..5";
            ".okay" = "^nope:1 ^rating:0..5";
            ".unrated" = "^rating:0..10 ^nope:1";
            ".decentSongs" = "^nope:1 rating:6.. length:..10:00";
            ".goodSongs" = "^nope:1 rating:8.. length:..10:00";
            ".greatSongs" = "^nope:1 rating:9.. length:..10:00";
            ".decentMixes" = "^nope:1 rating:6.. length:10:00..";
            ".goodMixes" = "^nope:1 rating:8.. length:10:00..";
            ".greatMixes" = "^nope:1 rating:9.. length:10:00..";
            ".now" = "now:1 ^rating:0..5 added:-6m..";
            ".SafariSound" = "Safari Sound";
            ".Orchestral" = "orchestral:1";
            artist = {
              Savant = "artist:Savant ^rating:0..5";
              ThomasBergersen = "artist:'Thomas Bergersen' ^rating:0..5";
              KatherineJenkins = "artist:'Katherine Jenkins' ^rating:0..5";
              ProleteR = "artist:'ProleteR' ^rating:0..5";
              Shadrew = "artist:Shadrew ^rating:0..5";
            };
            album = {
              ThomasBergersen = {
                Illusions = "album:'Illusions' ^rating:0..5";
                Sun = "album:'Sun' artist:'Thomas Bergersen' ^rating:0..5";
              };
              YourName = "album:'Your Name.' ^rating:0..5";
              Sigma = "album:'Σ' ^rating:0..5";
              CobaltHour = "album:'COBALT HOUR' ^rating:0..5";
              BenBriggs.Patreon = "artist:'Benjamin Briggs' album:'The Patreon Collection' ^rating:0..5";
              BenBriggs.DiddyKong = "artist:'Ben Briggs' album:'DIDDY KONG RACING: bootleg circuit' ^rating:0..5";
              Minecraft = "album:'Minecraft, Volume Beta' ^rating:0..5";
              Lustrous = "album:'TVアニメ『宝石の国』オリジナルサウンドトラック' ^rating:0..5";
              TreasurePlanet = "album:'Treasure Planet' ^rating:0..5";
              Symphonic.Shades = "album:'Symphonic Shades' ^rating:0..5";
              Symphonic.Fantasies = "album:'Symphonic Fantasies' ^rating:0..5";
              ComputerSavvy = "album:'Computer Savvy' ^rating:0..5";
              SpaceCadet = "album:'Space Cadet' ^rating:0..5";
              PrincessMononoke = "album:'Princess Mononoke' ^rating:0..5";
              Savant = lib.mapAttrs (n: v: "artist:Savant ^rating:0..5 ${v}") {
                Alchemist = "album:Alchemist";
                Heart = "album:'♥ (heart)'";
                Zion = "album:ZION";
                Protos = "album:Protos";
                Orakel = "album:Orakel";
                Overkill = "album:Overkill";
                Overworld = "album:Overworld";
                Jester = "album:Jester";
                Invasion = "album:Invasion";
                Cult = "album:Cult";
                Vario = "album:Vario";
              };
              Highlander = "album:Highlander ^rating:0..5";
              SecretGarden = "album:'Songs From a Secret Garden' ^rating:0..5";
            };
          };
        };
      };
    };

    users.users.nginx.extraGroups = [ "music" ];

    services.nginx = {
      enable = true;
      virtualHosts."music.infinisil.com" = {
        enableACME = true;
        forceSSL = true;
        basicAuth.infinisil = config.private.passwords."infinisil@media.infinisil.com";
        root = "${musicDir}/data";
        extraConfig = ''
          autoindex on;
          charset UTF-8;
        '';
      };
    };
  };

}
