{ nodes, ... }: {
  home-manager.users.infinisil = {

    programs.ssh = {
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
  };
}
