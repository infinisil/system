{ lib, config, pkgs, ... }:

with lib;

{

  options.mine.console.enable = mkOption {
    type = types.bool;
    default = true;
    description = "enable console config";
  };

  config = mkIf config.mine.console.enable {

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
        tasknc
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

      programs.vim = {
        enable = true;
        settings = {
          tabstop = 2;
          shiftwidth = 2;
        };
        plugins = [
          "YouCompleteMe"
          "ghc-mod-vim"
          "gundo"
          "colors-solarized"
          "vim-nix"
          "syntastic"
          "editorconfig-vim"
          "vim-pandoc"
        ];
        extraConfig = ''
          filetype on
          syntax enable
          filetype plugin indent on
          nnoremap j gj
          nnoremap k gk
          set hidden
          set termguicolors
          set showcmd
          set background=dark

          highlight OverLength ctermbg=red ctermfg=white guibg=#592929
          match OverLength /\%81v.\+/

          set hlsearch
          set undofile
          set undodir=$HOME/.local/share/vim/undo
          set directory=$HOME/.local/share/vim/swap
          set backup
          set backupdir=$HOME/.local/share/vim/backup
          set backspace=indent,eol,start
          set clipboard=unnamedplus
          set relativenumber

          let mapleader = " "

          let g:netrw_home='$HOME/.cache/vim'
        '';
      };

    };

  };


}
