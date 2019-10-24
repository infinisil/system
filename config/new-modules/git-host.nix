{ config, lib, ... }:

with lib;

{

  options.mine.gitHost.enable = mkEnableOption "use this machine as a host for git repos";

  config = mkIf config.mine.gitHost.enable {

    users.extraUsers.git = {
      isNormalUser = true;
      createHome = true;
      home = "/home/git";
      group = "git";
      description = "Git repository user";
    };

    users.groups.git.gid = null;

    users.extraUsers.infinisil.extraGroups = [ "git" ];

  };

}
