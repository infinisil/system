{ pkgs, config, lib, epkgs, dag, ... }:


with lib;

let

  hie = import (import ../../sources).hie-nix {};

  hiebin = pkgs.writeScriptBin "hie" ''
    #!${pkgs.stdenv.shell}
    export LD_LIBRARY_PATH="${lib.makeLibraryPath [ pkgs.gmp ]}:$LD_LIBRARY_PATH"
    exec ${pkgs.direnv}/bin/direnv exec . ${hie.hies}/bin/hie-wrapper "$@"
  '';

in

{

  options.haskell = mkOption {
    type = types.bool;
    default = true;
    description = "Haskell emacs stuff";
  };

  config = mkIf config.haskell {
    packages = with epkgs; [
      company
      company-lsp
      flycheck
      lsp-mode
      lsp-ui
      lsp-haskell
      haskell-mode
      company-quickhelp
      hasky-extensions
    ];

    init.hs = dag.entryAfter [ "pkgs" ] ''
      (setq lsp-haskell-process-path-hie "${hiebin}/bin/hie")
      (require 'lsp)
      (require 'lsp-haskell)
      (add-hook 'haskell-mode-hook #'lsp)

      (add-hook 'haskell-mode-hook (lambda () (haskell-indentation-mode nil)))

      (defun hasky-keys ()
        "Hasky extension key binds"
        (require 'hasky-extensions)
        (local-set-key (kbd "C-c C-y") #'hasky-extensions)
        )
      (add-hook 'haskell-mode-hook 'hasky-keys)
      ;(global-company-mode)
      ;(company-quickhelp-mode)
    '';
  };
}
