{ lib, config, pkgs, ... }:

with lib;

{

  options.mine.console.enable = mkOption {
    type = types.bool;
    default = true;
    description = "enable console config";
  };

  config = mkIf config.mine.console.enable {

    environment.pathsToLink = [ "/share/zsh" ];

    mine.vim.enable = true;

    environment.variables = {
      BC_ENV_ARGS = "-lq";
      BC_LINE_LENGTH = "0";
    };

    environment.systemPackages = with pkgs; [
      most
      direnv
      gitFull
      tmux
      lsof
      pass
      dnsutils
      gnupg
      taskwarrior
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
      nix-top
      fd
      gitAndTools.gh
      fzf
      sqliteInteractive
      gnumake
      exa
      asciinema
      ncdu
      whois
      exa
      aspellDicts.en
      bat
    ];

    users.defaultUserShell = pkgs.zsh;

    mine.userConfig = {

      home.file.".sqliterc".text = ''
        .headers on
        .mode column
        .prompt "> "
      '';

      home.sessionVariables = rec {
        WEECHAT_HOME = "$HOME/.config/weechat";
        LESSHISTFILE = "-";
      };

      programs.git = {
        enable = true;
        package = pkgs.gitFull;
        userName = "Silvan Mosberger";
        userEmail = "contact@infinisil.com";
        aliases.lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
        signing = {
          key = "0xE8F1E9EAD284E17D";
          #signByDefault = true;
          gpgPath = "${pkgs.gnupg}/bin/gpg";
        };
        iniContent.credential.helper = "store";
        iniContent."url \"git@github.com:\"".pushInsteadOf = "https://github.com/";
        iniContent.pull.ff = "only";
        iniContent.commit.verbose = true;
      };

    };

  };


}
