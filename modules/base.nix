{ pkgs, ...}:
{
  imports = [
    ./passwords.nix
  ];

  users.extraUsers.infinisil = {
    description = "Silvan Mosberger";
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "systemd-journal"
    ];
  };

  nix = {
    trustedUsers = [ "root" "@wheel" ];
  };

  nixpkgs.config.allowUnfree = true;

  programs.ssh.startAgent = true;

  time.timeZone = "Europe/Zurich";
  
  # links the contents of the directory where this file is located to /run/current-system/configuration
  system.extraSystemBuilderCmds = "ln -sv ${./.} $out/configuration";

  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    git
    ripgrep
    tree
    wget
    cacert
  ];
}
