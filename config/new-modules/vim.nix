{ pkgs, config, lib, ... }:

with lib;

let

  dag = import ../../external/home-manager/modules/lib/dag.nix { inherit lib; };

in

{

  options.mine.vim.enable = mkEnableOption "vim config";

  config.mine.userConfig = mkIf config.mine.vim.enable {

    home.activation.vimDirs = dag.dagEntryAfter [ "linkGeneration" ] ''
      mkdir -p $HOME/.local/share/vim/{undo,swap,backup}
    '';

    nixpkgs.overlays = [(self: super: {
      ycmd = super.ycmd.override {
        gocode = null;
        godef = null;
        rustracerd = null;
      };
    })];

    programs.vim = {
      enable = true;
      settings = {
        tabstop = 2;
        shiftwidth = 2;
      };
      plugins = with pkgs.vimPlugins; [
        YouCompleteMe
        ghc-mod-vim
        gundo
        colors-solarized
        vim-nix
        syntastic
        editorconfig-vim
        vim-pandoc
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

}
