{ nodes, pkgs, ... }:

{
  mine.userConfig.programs.ssh = {
    enable = true;
    controlMaster = "auto";
    controlPersist = "60";
    matchBlocks = {
      protos = {
        hostname = "infinisil.com";
      };
      git = {
        hostname = "infinisil.com";
        user = "git";
      };
      ninur.proxyCommand = "${pkgs.bash}/bin/bash -c '/run/wrappers/bin/ping 192.168.178.53 -w1 && ${pkgs.netcat}/bin/nc 192.168.178.53 22 || ${pkgs.netcat}/bin/nc 10.149.76.3'";
      vario.proxyCommand = "${pkgs.bash}/bin/bash -c '/run/wrappers/bin/ping 192.168.178.28 -w1 && ${pkgs.netcat}/bin/nc 192.168.178.28 22 || ${pkgs.netcat}/bin/nc 10.149.76.2'";
    };
  };

}
