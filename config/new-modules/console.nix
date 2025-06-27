{ lib, config, pkgs, sources, ... }:

with lib;

{

  imports = [
    (import sources.flake-compat {
      src = sources.nix-index-database;
    }).defaultNix.nixosModules.nix-index
  ];

  options.mine.console.enable = mkOption {
    type = types.bool;
    default = true;
    description = "enable console config";
  };

  options.mine.console.users = mkOption {
    type = types.listOf types.str;
    default = [];
  };

  config = mkIf config.mine.console.enable {

    # Needed for the nix-index-database nixos module
    _module.args.databases = import (sources.nix-index-database + "/packages.nix");

    # Also enable comma
    programs.nix-index-database.comma.enable = true;

    # Conflicts with the nix-index-provided one
    programs.command-not-found.enable = false;

    environment.pathsToLink = [ "/share/zsh" ];

    mine.vim.enable = true;
    mine.vim.users = config.mine.console.users;

    programs.direnv.enable = true;

    environment.variables = {
      BC_ENV_ARGS = "-lq";
      BC_LINE_LENGTH = "0";
    };

    environment.systemPackages = with pkgs; [
      most
      gitFull
      tmux
      lsof
      pass
      dnsutils
      gnupg
      taskwarrior3
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
      fd
      gitAndTools.gh
      fzf
      sqlite-interactive
      gnumake
      eza
      asciinema
      ncdu
      whois
      aspellDicts.en
      bat
    ];

    users.defaultUserShell = pkgs.zsh;

    # Prevent the new user dialog in zsh
    system.userActivationScripts.zshrc = "touch .zshrc";

    home-manager.users = lib.genAttrs config.mine.console.users (user: {

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
        userEmail = lib.mkDefault "git@infinisil.com";
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
        iniContent.merge.conflictstyle = "zdiff3";
        iniContent.init.defaultBranch = "main";
      };

    });

  };


}
