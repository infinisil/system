{ lib, config, pkgs, mylib, ... }:

with lib;

let

  dag = mylib.dag;

in

{

  mine.emacs = {
    packages = with pkgs.epkgs; [
      better-defaults
      neotree
      gruvbox-theme
      which-key
      editorconfig
    ];

    init = {
      theme = dag.entryAfter [ "pkgs" ] ''
        (load-theme 'gruvbox t)
      '';

      base = dag.entryAfter [ "theme" ] ''
        (require 'better-defaults)

        (editorconfig-mode 1)
        ${lib.optionalString config.mine.emacs.usePretest "(pixel-scroll-mode)"}

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
