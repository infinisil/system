{ lib, config, pkgs, ... }:

with lib;

{

  options.mine.console.enable = mkOption {
    type = types.bool;
    default = true;
    description = "enable console config";
  };

  config = mkIf config.mine.console.enable {

    mine.vim.enable = true;

    programs.command-not-found.enable = true;

    environment.pathsToLink = [ "/share/zsh" ];

    programs.zsh = {
      enable = true;
      enableCompletion = false;
    };

    environment.systemPackages = with pkgs; [
      direnv
      nix-zsh-completions
      gitFull
      tmux
      lsof
      pass
      dnsutils
      gnupg
      taskwarrior
      tldr
      mtr
      iftop
      atool
      unzip
      jq
      bc
      wget
      ripgrep
      tree
      file
      mediainfo
      tokei
      nmap
      traceroute
    ];

    users.defaultUserShell = pkgs.zsh;

    mine.userConfig = {

      home.packages = with pkgs; [
        ranger
        gnupg
        fd
        tig
        gitAndTools.hub
        fzf
        sqliteInteractive
        gnumake
        exa
        asciinema
        cmatrix
        ncdu
        whois
        exa
        aspellDicts.en
        expect
        ansifilter
      ];

      home.file.".sqliterc".text = ''
        .headers on
        .mode column
        .prompt "> "
      '';

      home.sessionVariables = rec {
        XDG_CONFIG_HOME = "$HOME/.config";
        XDG_DATA_HOME = "$HOME/.local/share";
        XDG_CACHE_HOME = "$HOME/.cache";
        WEECHAT_HOME = "$HOME/.config/weechat";
        LESSHISTFILE = "-";
      };

      programs.git = {
        enable = true;
        package = pkgs.gitFull;
        userName = "Silvan Mosberger";
        userEmail = "infinisil@icloud.com";
        aliases = {
          lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
        };
        signing = {
          key = "5B2CFBD8E6AD7FC113D675A89424360B4B85C9E7";
          signByDefault = true;
          gpgPath = "${pkgs.gnupg}/bin/gpg";
        };
      };

      programs.command-not-found.enable = true;

    };

  };


}
