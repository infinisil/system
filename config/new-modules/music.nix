{ config, lib, pkgs, ...}:

with lib;

let

  cfg = config.mine.music;

  serverConfig = if (config.mine.music.client.server == null)
    then config else config.mine.music.client.server.config;

  serverPassword = serverConfig.mine.music.server.password;

  serverIp = (head serverConfig.networking.interfaces.eth0.ipv4.addresses).address;

  serverPort = serverConfig.mine.music.server.port;

  serverDomain = serverConfig.networking.domain;

in

{

  options.mine = {
    mpdHost = mkOption {
      type = types.str;
    };

    mpdPort = mkOption {
      type = types.str;
    };
  };


  config = {

    mine.mpdHost = "${serverPassword}@${serverIp}";
    mine.mpdPort = "${toString serverPort}";

    mine.userConfig = mkIf cfg.client.enable (mkMerge [
      (mkIf (!cfg.server.enable) {

        home.packages = with pkgs; [
          (writeScriptBin "beet" ''
            #!${stdenv.shell}
            ssh -q -t ${serverIp} "MPD_HOST=${config.mine.mpdHost} beet ''${@@Q}"
          '')
        ];

      })
      {

        home.packages = with pkgs; [
          mpc_cli
          ncmpcpp
        ];

        programs.zsh.shellAliases.beet = "noglob beet";

        home.sessionVariables = {
          MPD_HOST = config.mine.mpdHost;
          MPD_PORT = config.mine.mpdPort;
        };

      }
      (mkIf cfg.client.listen {

        systemd.user.services.music = {
          Unit = {
            Description = "Play music";
            After = [ "graphical-session-pre.target" "network.target" ];
          };

          Service = {
            ExecStart = "${pkgs.mpv}/bin/mpv https://tune.${serverDomain}/opus --quiet";
            Restart = "on-success";
            SuccessExitStatus = 4;
            RestartPreventExitStatus = 4;
          };
        };

      })
    ]);
  };
}
