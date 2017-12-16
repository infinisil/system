{ pkgs, ... }: {

  home-manager.users.infinisil = let
    ghc = pkgs.haskellPackages.ghcWithPackages (hp: with hp; [
      xmonad
      xmonad-contrib
      fuzzy
      aeson
      network-simple
    ]);

    ghc-mod-wrapped = let
      ghc-mod = pkgs.haskellPackages.ghc-mod;
    in pkgs.runCommand "ghc-mod-wrapped" {
      buildInputs = [ pkgs.makeWrapper ];
    } ''
      mkdir $out
      ln -s ${ghc-mod}/* $out

      rm $out/bin
      mkdir $out/bin
      ln -s ${ghc-mod}/bin/* $out/bin
      rm $out/bin/ghc-mod
      makeWrapper ${ghc-mod}/bin/ghc-mod $out/bin/ghc-mod \
        --run 'export NIX_GHC_LIBDIR="$(ghc --print-libdir)"'
        #--set NIX_GHC_LIBDIR "$(${ghc}/bin/ghc --print-libdir)"
    '';
  in {

    home.packages = with pkgs; [
      ghc
      ghc-mod-wrapped
      haskellPackages.stylish-haskell
      haskellPackages.hasktags
      haskellPackages.hindent
    ];

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

        haskell-mode
        ghc-mod
        hindent

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
