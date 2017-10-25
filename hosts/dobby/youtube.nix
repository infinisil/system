{ config, lib, pkgs, ... }:

let

  dataDir = "/home/infinisil/.local/share/youtube";

in

{

  systemd.services.youtube = {
    description = "Automatic youtube download";
    wantedBy = [ "multi-user.target" ];

    preStart = ''
      mkdir -p ${dataDir}
    '';

    serviceConfig = {
      User = "infinisil";
      Environment = "MPD_HOST=${config.passwords.mpd}@infinisil.com";
      Restart = "always";
      RestartSec = 10;
      ExecStart = let
        exec = pkgs.writeScript "exec" ''
          #!${pkgs.bash}/bin/bash
          echo Importing item
          ${pkgs.beets}/bin/beet import -sAI --set=now=1 "$1" 2>/dev/null
          echo Writing tags to file
          ${pkgs.beets}/bin/beet write now:1 2>/dev/null
          echo Waiting for mpd to finish updating
          sleep 1
          echo Playing now:1
          ${pkgs.beets}/bin/beet play -y now:1 2>/dev/null
        '';
        args = lib.concatStringsSep " " [
          "-x"
          "--download-archive ${dataDir}/ids"
          "--add-metadata"
          "-o \"${dataDir}/videos/%(title)s.%(ext)s\""
          "--exec '${exec} {}'"
          (import ../../private/youtube.nix).youtubeDownloadPlaylist
        ];
        youtube-script = pkgs.writeScriptBin "youtube-script" ''
          #!${pkgs.bash}/bin/bash
          while true; do
            echo Checking for new videos
            ${pkgs.youtube-dl}/bin/youtube-dl ${args}
            echo Sleeping for 10 seconds
            sleep 10
          done
        '';
      in "${youtube-script}/bin/youtube-script";
    };
  };
}
