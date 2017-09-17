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
    shellAliases = {
      nixrc = "vim /cfg/system/nixos";
    };
    ohMyZsh = let 
      packages = [
        {
          owner = "bhilburn";
          repo = "powerlevel9k";
          rev = "v0.6.3";
          sha256 = "026j7ryzmrqhjd72b0axiz9bi63bfbzl378f9fn7y5g9bdy641qk";
        }
      ];

      fetchToFolder = { repo, ...}@attrs:
        pkgs.fetchFromGitHub (attrs // {
          extraPostFetch = ''
            tmp=$(mktemp -d)
            mv $out/* $tmp
            mkdir $out/${repo}
            mv $tmp/* $out/${repo}
          '';
        });
      custom = pkgs.buildEnv {
        name = "zsh-custom";
        paths = builtins.map fetchToFolder packages;
      };
    in
    {
      enable = true;
      custom = custom.outPath;
      theme = "powerlevel9k/powerlevel9k";
      plugins = [ "git" "pass" "brew" "colored-man" "colorize" ];
    };
  };

  users.defaultUserShell = pkgs.zsh;
}
