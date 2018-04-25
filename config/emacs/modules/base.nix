{ lib, config, epkgs, dag, ... }: {

  packages = with epkgs; [
    better-defaults
    neotree
    gruvbox-theme
    which-key
    editorconfig
    flycheck
  ];

  init = {
    theme = dag.entryAfter [ "pkgs" ] ''
      (load-theme 'gruvbox t)
    '';

    base = dag.entryAfter [ "theme" ] ''
      (require 'better-defaults)

      (editorconfig-mode 1)
      ${lib.optionalString config.usePretest "(pixel-scroll-mode)"}

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

      (add-hook 'after-init-hook #'global-flycheck-mode)

      (setenv "NIX_REMOTE" "daemon")

      (setq custom-file (concat (getenv "HOME") "/.emacs.d/custom.el"))
      (load custom-file)

      (global-visual-line-mode)
    '';
  };
}
