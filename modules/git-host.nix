{ config, pkgs, ... }:

{
  users.extraUsers.git = {
    isNormalUser = true;
    createHome = true;
    home = "/home/git";
    group = "git";
    description = "Git repository user";
    openssh.authorizedKeys.keys = config.allsshkeys;
  };

  users.groups.git.gid = null;

  users.extraUsers.infinisil.extraGroups = [ "git" ];
}
