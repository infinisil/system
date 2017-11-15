{ config, pkgs, ... }:
{
  programs.command-not-found.enable = true;

  programs.zsh = {
    enable = true;
    enableCompletion = false;
  };

  environment.systemPackages = with pkgs; [
    git
    lsof
    pass
    gnupg
    taskwarrior
    tldr
    mtr
    nix-repl
    iftop
    atool
    jq
    bc
    wget
    ripgrep
    tree
  ];

  users.defaultUserShell = pkgs.zsh;
}
