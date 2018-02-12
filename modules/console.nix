{ config, pkgs, ... }:
let

  home = {

    manual.manpages.enable = true;

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
      XDG_CONFIG_HOME = "${config.home-manager.users.infinisil.home.homeDirectory}/.config";
      XDG_DATA_HOME = "${config.home-manager.users.infinisil.home.homeDirectory}/.local/share";
      XDG_CACHE_HOME = "${config.home-manager.users.infinisil.home.homeDirectory}/.cache";
      MPD_HOST = "${config.private.passwords.mpd}@infinisil.com";
      WEECHAT_HOME = "${XDG_CONFIG_HOME}/weechat";
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

    programs.vim = with config.home-manager.users.infinisil.home.sessionVariables; {
      enable = true;
      settings = {
        tabstop = 2;
        shiftwidth = 2;
      };
      plugins = [
        "YouCompleteMe"
        "ghc-mod-vim"
        "gundo"
        "vim-racer"
        "colors-solarized"
        "rust-vim"
        "vim-nix"
        "syntastic"
        "editorconfig-vim"
        "vim-pandoc"
      ];
      extraConfig = let
        rustSource = pkgs.fetchFromGitHub {
          owner = "rust-lang";
          repo = "rust";
          rev = "1.19.0";
          sha256 = "0r4al3yra15cjqqdhixcbhaxqaq8wcza4mirgj143ns6cb0vjzpq";
        };
      in ''
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
        set undodir=${XDG_DATA_HOME}/vim/undo
        set directory=${XDG_DATA_HOME}/vim/swap
        set backup
        set backupdir=${XDG_DATA_HOME}/vim/backup
        set backspace=indent,eol,start
        set clipboard=unnamedplus
        set relativenumber

        let mapleader = " "

        let g:netrw_home='${XDG_CACHE_HOME}/vim'
        let g:racer_cmd = "${pkgs.rustracer}/bin/racer"
        let $RUST_SRC_PATH="${rustSource}"
      '';
    };

    programs.htop = {
      enable = true;
      fields = [
        "PID" "USER" "PRIORITY"
        "PERCENT_CPU" "M_RESIDENT" "PERCENT_MEM"
        "TIME" "COMM"
      ];
      colorScheme = 0;
      hideThreads = true;
      hideUserlandThreads = true;
      showProgramPath = false;
      highlightBaseName = true;
    };

  };

in
{
  imports = [
    ./zsh.nix
  ];

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

  home-manager.users.infinisil = home;
}
