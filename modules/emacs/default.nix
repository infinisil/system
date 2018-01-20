{ pkgs, ... }: {

  home-manager.users.infinisil = {

    home.packages = with pkgs; [
      haskellPackages.haskell-ide-engine
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

        flycheck
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
