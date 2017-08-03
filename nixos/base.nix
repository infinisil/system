{ config, pkgs, ...}:
{
  users.extraUsers.infinisil = {
    isNormalUser = true;
    home = "/home/infinisil";
    description = "Silvan Mosberger";
    extraGroups = [
      "wheel"
    ];
  };

  nixpkgs.config.allowUnfree = true;
  nix.useSandbox = true;

  programs.ssh.startAgent = true;

  time.timeZone = "Europe/Zurich";

  environment.systemPackages = with pkgs; [
    git
    nix-repl
    ripgrep
    neovim
  ];
}
