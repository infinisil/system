{ lib, pkgs, config, epkgs, dag, ... }:

with lib;

let

  schemas = pkgs.writeText "schemas.xml" ''
    <locatingRules xmlns="http://thaiopensource.com/ns/locating-rules/1.0">
      <documentElement localName="section" typeId="DocBook"/>
      <documentElement localName="chapter" typeId="DocBook"/>
      <documentElement localName="article" typeId="DocBook"/>
      <documentElement localName="book" typeId="DocBook"/>
      <typeId id="DocBook" uri="${pkgs.docbook5}/xml/rng/docbook/docbookxi.rnc" />
    </locatingRules>
  '';

  nix-docbook-mode = epkgs.trivialBuild {
    pname = "nix-docbook-mode";
    version = "1970-01-01";
    src = pkgs.writeText "default.el" ''
      (eval-after-load 'rng-loc
        '(add-to-list 'rng-schema-locating-files "${schemas}"))
    '';
  };

in
{

  packages = with epkgs; [
    #use-package
    #dashboard
    #doom-themes
    #highlight-numbers
    #evil
    #evil-collection
    #evil-commentary
    #evil-magit
    #magit
    #company
    #ido-vertical-mode
    #epkgs."ido-completing-read+"
    #flx-ido
    #flycheck
    #json-mode
    #diminish
    #which-key
    #flycheck-posframe
    #direnv
    #editorconfig

    #haskell-mode
    #lsp-haskell
    #lsp-mode
    #hasky-extensions
    #lsp-mode
    #lsp-ui

    #yasnippet
    #company-posframe
    ##better-defaults
    #projectile
    #all-the-icons
    ##neotree
    #gruvbox-theme
    #which-key
    #editorconfig
    #flycheck
    #company
    ##magit
    #smooth-scrolling
    ##helm
    #direnv

    nix-mode
  ] ++ [ nix-docbook-mode ];

  init = {
    #theme = ''
    #  (load-theme 'gruvbox t)
    #'';

    #base = dag.entryAfter [ "theme" ] ''

    #  (setq inhibit-startup-screen t)
    #  (require 'better-defaults)

    #  (require 'direnv)

    #  (editorconfig-mode 1)

    #  (setq company-minimum-prefix-length 1)
    #  (setq company-idle-delay 0.1)
    #  (global-company-mode)

    #  (require 'recentf)
    #  (recentf-mode)
    #  (which-key-mode)

    #  (let
    #      ((homepath (concat (getenv "HOME") "/.nix-profile/bin"))
    #       (systempath "/run/current-system/sw/bin"))
    #      (setenv "PATH" (concat homepath ":" systempath ":" (getenv "PATH")))
    #      (add-to-list 'exec-path systempath)
    #      (add-to-list 'exec-path homepath)
    #      )

    #  (setq-default ispell-program-name "${pkgs.hunspellWithDicts [ pkgs.hunspellDicts.en-us ]}/bin/hunspell")
    #  (add-hook 'text-mode-hook 'flyspell-mode)

    #  (add-hook 'after-init-hook #'global-flycheck-mode)

    #  (setenv "NIX_REMOTE" "daemon")

    #  (setq custom-file (concat (getenv "HOME") "/.emacs.d/custom.el"))
    #  (load custom-file)

    #  (global-visual-line-mode)

    #  (defun gcm-scroll-down ()
    #    (interactive)
    #    (scroll-up 1))

    #  (defun gcm-scroll-up ()
    #    (interactive)
    #    (scroll-down 1))

    #  (global-set-key "\C-j" 'gcm-scroll-down)
    #  (global-set-key "\C-k" 'gcm-scroll-up)

    #  (require 'smooth-scrolling)
    #  (smooth-scrolling-mode 1)

    #  (setq mouse-wheel-scroll-amount '(5 ((shift) . 1))) ;; one line at a time
    #  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

    #  (load (concat (getenv "HOME") "/.emacs.d/dinit.el"))
    #'';
  };
}
