{ nodes, pkgs, ... }:

let
  options = {
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
      laptop.proxyCommand = "${pkgs.bash}/bin/bash -c '/run/wrappers/bin/ping 192.168.1.19 -w1 && ${pkgs.netcat}/bin/nc 192.168.1.19 22 || ${pkgs.netcat}/bin/nc infinisil.com ${toString nodes.laptop.config.localserver.sshport}'";
      pc.proxyCommand = "${pkgs.bash}/bin/bash -c '/run/wrappers/bin/ping 192.168.1.25 -w1 && ${pkgs.netcat}/bin/nc 192.168.1.25 22 || ${pkgs.netcat}/bin/nc infinisil.com ${toString nodes.pc.config.localserver.sshport}'";
    };
  };

in

{

  home-manager.users.infinisil.programs.ssh = options;
  home-manager.users.root.programs.ssh = options;

}
