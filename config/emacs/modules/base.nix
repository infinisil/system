{ lib, config, epkgs, dag, ... }:

with lib;

{

  options.base = mkOption {
    type = types.bool;
    default = true;
    description = "Whether to enable base";
  };

  config = mkIf config.base {

    packages = with epkgs; [
      better-defaults
      neotree
      gruvbox-theme
      which-key
      editorconfig
      flycheck
      company
      magit
      smooth-scrolling
      direnv
      helm
    ];

    init = {
      theme = dag.entryAfter [ "pkgs" ] ''
        (load-theme 'gruvbox t)
        (direnv-mode)
      '';

      base = dag.entryAfter [ "theme" ] ''
        (require 'better-defaults)

        (editorconfig-mode 1)

        (setq company-minimum-prefix-length 1)
        (setq company-idle-delay 0.3)
        (global-company-mode)

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
        (load (concat (getenv "HOME") "/.emacs.d/dinit.el"))
      '';
    };

  };
}
