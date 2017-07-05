{ config, pkgs, ...}:
{
  users.extraUsers.git = {
    isNormalUser = true;
    createHome = true;
    home = "/home/git";
    group = "git";
    description = "Git repository user";
  };

  users.groups.git.gid = null;

  users.extraUsers.infinisil = {
    isNormalUser = true;
    home = "/home/infinisil";
    description = "Silvan Mosberger";
    extraGroups = [
      "fuse"
      "wheel"
      "networkmanager"
      "ipfs"
      "systemd-journal"
      "nginx"
    ];
  };

  
}
