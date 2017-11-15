{ pkgs, ... }:

{
  networking.firewall.allowedUDPPorts = [ 4446 ];

  systemd.services.chat = {
    description = "Chat service for Distributed Systems";

    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      ExecStart = "${pkgs.zulu}/bin/java -jar ${./chat_server.jar}";
      Restart = "on-failure";
      RestartSec = 1;
    };
  };
}
