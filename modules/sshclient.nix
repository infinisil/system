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
      laptop = {
        hostname = "infinisil.com";
        port = nodes.laptop.config.localserver.sshport;
      };
      pc = {
        hostname = "infinisil.com";
        port = nodes.pc.config.localserver.sshport;
      };
    };
  };

in

{

  home-manager.users.infinisil.programs.ssh = options;
  home-manager.users.root.programs.ssh = options;

}
