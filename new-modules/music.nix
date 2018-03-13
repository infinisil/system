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

  config = mkIf cfg.client.enable (mkMerge [
    {

      environment.systemPackages = with pkgs; [
        mpc_cli
        ncmpcpp
        (pkgs.writeScriptBin "beet" ''
          #!${pkgs.stdenv.shell}
          ssh -q -t ${serverIp} "beet ''${@@Q} 2>/dev/null"
        '')
      ] ++ lib.optional config.services.xserver.enable pkgs.sonata;

      environment.variables = {
        MPD_HOST = "${serverPassword}@${serverIp}";
        MPD_PORT = "${toString serverPort}";
      };

    }
    (mkIf cfg.client.listen {


      mine.userConfig = {
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
      };

    })
  ]);
}
