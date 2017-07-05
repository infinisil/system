{ config, pkgs, ...}:
{
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
    shell = pkgs.zsh;
  };
}
