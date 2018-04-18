{ lib, config, pkgs, mylib, ... }:

with lib;

let

  dag = mylib.dag;

in

{

  mine.emacs = {
    packages = with pkgs.emacsPackagesNg; [
      better-defaults
      neotree
      gruvbox-theme
      which-key
    ];

    init = {
      theme = dag.entryAfter [ "pkgs" ] ''
        (load-theme 'gruvbox t)
      '';

      base = dag.entryAfter [ "theme" ] ''
        (require 'better-defaults)

        (require 'recentf)
        (recentf-mode)
        (which-key-mode)

        (let
            ((homepath (concat (getenv "HOME") "/.nix-profile/bin"))
             (systempath "/run/current-system/sw/bin"))
            (setenv "PATH" (concat homepath ":" systempath ":" (getenv "PATH")))
            (add-to-list 'exec-path systempath)
            (add-to-list 'exec-path homepath)
            )

        (setenv "NIX_REMOTE" "daemon")

        (setq custom-file (concat (getenv "HOME") "/.emacs.d/custom.el"))
        (load custom-file)

        (global-visual-line-mode)
      '';
    };
  };
}
