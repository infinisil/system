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
    file
    mediainfo
    loc
  ];

  users.defaultUserShell = pkgs.zsh;
}
