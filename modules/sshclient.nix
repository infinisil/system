{
  home-manager.users.infinisil = {

    programs.ssh = {
      enable = true;
      controlMaster = "yes";
      matchBlocks = {
        inf = {
          hostname = "infinisil.com";
        };
        git = {
          hostname = "infinisil.com";
          user = "git";
        };
      };
    };
  };
}
