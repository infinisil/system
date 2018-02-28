{ pkgs, ... }: let

  haskell-ide-engine = (import (pkgs.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "7dbd28563198c33b17ae9b5ebabf6c0a08d21953";
    sha256 = "1mq2vll2mq0bkb2xg8874dyvq8dakaqf1lnz5n0i23s39bldjdmr";
  }) {}).hie82;

in
  {

  home-manager.users.infinisil = {

    home.packages = with pkgs; [
      haskell-ide-engine
      haskell.compiler.ghc822
    ];

    home.file.".emacs.d/init.el".source = "${pkgs.mine.emacs.init}/init.el";

    programs.emacs = {
      enable = true;
      enableDaemon = true;
      package = pkgs.mine.emacs.emacs;
    };
  };

}
