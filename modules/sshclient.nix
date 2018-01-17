{ nodes, ... }:

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
      laptop.proxyCommand = "sh -c 'ping 192.168.1.19 -w1 && nc 192.168.1.19 22 || nc infinisil.com ${toString nodes.laptop.config.localserver.sshport}'";
      pc.proxyCommand = "sh -c 'ping 192.168.1.25 -w1 && nc 192.168.1.25 22 || nc infinisil.com ${toString nodes.pc.config.localserver.sshport}'";
    };
  };

in

{

  home-manager.users.infinisil.programs.ssh = options;
  home-manager.users.root.programs.ssh = options;

}
