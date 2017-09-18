{ config, pkgs, ... }:
{
  imports = [
    ./keylayout.nix
  ];

  programs.command-not-found.enable = true;

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;
  };

  users.defaultUserShell = pkgs.zsh;
}
