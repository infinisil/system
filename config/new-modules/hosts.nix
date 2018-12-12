{ nodes, pkgs, ... }:

{
  mine.userConfig.programs.ssh = {
    enable = true;
    controlMaster = "auto";
    controlPersist = "60";
    matchBlocks = {
      inf = {
        hostname = "infinisil.com";
      };
      git = {
        hostname = "infinisil.com";
        user = "git";
      };
      kuro = {
        hostname = "kuro.desu.tk";
        user = "infinisil";
      };
      laptop.proxyCommand = "${pkgs.bash}/bin/bash -c '/run/wrappers/bin/ping 192.168.178.53 -w1 && ${pkgs.netcat}/bin/nc 192.168.178.53 22 || ${pkgs.netcat}/bin/nc infinisil.com ${toString nodes.emma.config.mine.rpf.client.sshport}'";
      pc.proxyCommand = "${pkgs.bash}/bin/bash -c '/run/wrappers/bin/ping 192.168.178.28 -w1 && ${pkgs.netcat}/bin/nc 192.168.178.28 22 || ${pkgs.netcat}/bin/nc infinisil.com ${toString nodes.nepnep.config.mine.rpf.client.sshport}'";
    };
  };

}
