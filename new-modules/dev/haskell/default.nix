{ lib, config, pkgs, mylib, ... }:

with lib;

let

  dag = mylib.dag;

  hie-nix = pkgs.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "dbb89939da8997cc6d863705387ce7783d8b6958";
    sha256 = "1bcw59zwf788wg686p3qmcq03fr7bvgbcaa83vq8gvg231bgid4m";
  };

  stack2nixSrc = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "stack2nix";
    rev = "b614c56ea3c36d9f5940fb1852b7957cad586547";
    sha256 = "1nk3pax43zz15lqn2j963zkn9x9na7q2kid9vk5kxif9daymq4wd";
  };

  hie = import hie-nix {};

  myStack2nix = import "${stack2nixSrc}" {};

in

{

  options.mine.dev.haskell.enable = mkEnableOption "Haskell dev config";

  config = mkIf config.mine.dev.haskell.enable (mkMerge [
    {
      environment.systemPackages = with pkgs; [
        haskell.compiler.ghc822
        hie.hie82

        stack
        myStack2nix

        cabal2nix
        cabal-install
      ];
    }
    (mkIf config.mine.emacs.enable {
      mine.emacs = {

        packages = with pkgs.epkgs; [
          company
          flycheck
          lsp-mode
          lsp-ui
          lsp-haskell
          haskell-mode
          company-quickhelp
        ];

        init.hs = dag.entryAfter [ "pkgs" ] ''
          (require 'lsp-ui)
          (require 'lsp-haskell)
          (add-hook 'lsp-mode-hook 'lsp-ui-mode)
          (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
          (add-hook 'haskell-mode-hook 'flycheck-mode)
          ;(require 'company-lsp)
          ;(push 'company-lsp company-backends)
          (setq company-minimum-prefix-length 1)
          (setq company-idle-delay 0)
          (global-company-mode)
          (company-quickhelp-mode)
        '';
      };
    })
  ]);

}
