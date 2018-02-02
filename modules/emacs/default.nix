{ pkgs, ... }: let

  haskell-ide-engine = (import (pkgs.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "42fe84af0a0bed1251774cf42805c42f01ecfa12";
    sha256 = "1n2f8icmlsamg65j4q1wagpcnclah6ykjnnn2x45rf5i7qfdi6x0";
  }))."hie-8.2";

in
  {

  home-manager.users.infinisil = {

    home.packages = with pkgs; [
      haskell-ide-engine
      haskell.compiler.ghc822
    ];

    home.file.".emacs.d/init.el".text = ''
      (package-initialize)

      (setq custom-file "${./custom.el}")
      (load custom-file)
      (setq custom-file "${toString ./custom.el}")

      ${builtins.readFile ./init.el}
    '';


    programs.emacs = {
      enable = true;
      enableDaemon = true;
      extraPackages = epkgs: with epkgs; [
        evil
        magit
        color-theme
        powerline
        solarized-theme
        frames-only-mode
        which-key
        evil-leader
        guide-key
        org
        htmlize

        company
        company-ghc
        melpaPackages.company-lsp

        flycheck
        flycheck-haskell
        ghc
        lsp-mode
        lsp-haskell
        lsp-ui
        haskell-mode

        better-defaults

        nix-mode
        nixos-options
        helm-nixos-options
        company-nixos-options
        nix-sandbox

        idris-mode

        gruvbox-theme
        rainbow-mode

        minimap

        glsl-mode
        markdown-mode
        brainfuck-mode
        neotree
      ];
    };
  };

}
