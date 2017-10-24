{ config, lib, pkgs, ... }:

let

  dataDir = "/home/infinisil/.local/share/youtube";

in

{

  systemd.services.youtube = {
    description = "Automatic youtube download";

    preStart = ''
      mkdir -p ${dataDir}
    '';

    serviceConfig = {
      User = "infinisil";
      Environment = "MPD_HOST=${config.passwords.mpd}@infinisil.com";
      ExecStart = let
        exec = pkgs.writeScript "exec" ''
          #!${pkgs.bash}/bin/bash
          ${pkgs.beets}/bin/beet -v import -sAI --set=now=1 "$1"
          ${pkgs.beets}/bin/beet play -y now:1
        '';
        args = lib.concatStringsSep " " [
          "-x"
          "--download-archive ${dataDir}/ids"
          "--add-metadata"
          "-o \"${dataDir}/videos/%(title)s.%(ext)s\""
          "--exec '${exec} {}'"
          (import ../../private/youtube.nix).youtubeDownloadPlaylist
        ];
      in "${pkgs.youtube-dl}/bin/youtube-dl ${args}";
    };
  };

  systemd.timers.youtube = {
    description = "Timer for automatic youtube download";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      Persistent = "true";
      OnBootSec = "10";
      OnUnitActiveSec = "10";
    };
  };

}
