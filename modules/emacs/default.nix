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

    home.file.".emacs.d/init.el".source = "${pkgs.mine.emacs.init}/init.el";

    programs.emacs = {
      enable = true;
      enableDaemon = true;
      package = pkgs.mine.emacs.emacs;
    };
  };

}
