{ pkgs, ... }: {

  home-manager.users.infinisil = let
    ghc = pkgs.haskellPackages.ghcWithPackages (hp: with hp; [
      xmonad
      xmonad-contrib
      fuzzy
      aeson
      network-simple
    ]);

    ghc-mod-wrapped = let
      ghc-mod = pkgs.haskellPackages.ghc-mod;
    in pkgs.runCommand "ghc-mod-wrapped" {
      buildInputs = [ pkgs.makeWrapper ];
    } ''
      mkdir $out
      ln -s ${ghc-mod}/* $out

      rm $out/bin
      mkdir $out/bin
      ln -s ${ghc-mod}/bin/* $out/bin
      rm $out/bin/ghc-mod
      makeWrapper ${ghc-mod}/bin/ghc-mod $out/bin/ghc-mod \
        --run 'export NIX_GHC_LIBDIR="$(ghc --print-libdir)"'
        #--set NIX_GHC_LIBDIR "$(${ghc}/bin/ghc --print-libdir)"
    '';
  in {

    home.packages = with pkgs; [
      ghc
      ghc-mod-wrapped
      haskellPackages.stylish-haskell
      haskellPackages.hasktags
      haskellPackages.hindent
    ];

    home.file.".emacs.d/init.el".text = ''
      (package-initialize)

      (setq custom-file "~/.emacs.d/custom.el")
      (load custom-file)

      (evil-mode 1)
      (add-hook 'neotree-mode-hook
        (lambda ()
          (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
          (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
          (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
          (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

      (require 'better-defaults)

      (global-evil-leader-mode)
      (require 'evil-leader)
      (require 'which-key)
      (require 'recentf)
      (recentf-mode)
      (require 'helm-config)


      (evil-leader/set-leader "<SPC>")
      (which-key-add-key-based-replacements "SPC b" "buffer")
      (evil-leader/set-key "b k" 'kill-buffer)
      (evil-leader/set-key "b s" 'buffer-menu)
      (evil-leader/set-key "TAB" 'next-buffer)

      (which-key-add-key-based-replacements "SPC e" "eval")
      (evil-leader/set-key "e r" 'eval-region)
      (evil-leader/set-key "e l" (defun eval-line() (interactive) (eval-region (line-beginning-position) (line-end-position))))

      (which-key-add-key-based-replacements "SPC f" "file")
      (evil-leader/set-key "f c" (defun emacs-config() (interactive) (find-file user-init-file)))
      (evil-leader/set-key "f h" (defun habits() (interactive) (find-file "~/habits/main.org")))
      (evil-leader/set-key "f f" 'find-file)
      (evil-leader/set-key "f r" 'recentf-open-files)
      (evil-leader/set-key "f s" 'save-buffer)


      (which-key-add-key-based-replacements "SPC w" "window")
      (evil-leader/set-key "w o" 'delete-other-windows)
      (evil-leader/set-key "w k" 'delete-windows)

      ;(setq evil-leader/in-all-states t)
      (which-key-mode)

      (require 'company-ghc)
      (add-to-list 'company-backends 'company-ghc)
      ;(add-to-list 'company-backends 'company-nixos-options)
      ;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
      (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
      (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
      (add-hook 'haskell-mode-hook 'haskell-doc-mode)
      (add-hook 'after-init-hook 'global-company-mode)

      (let
          ((homepath (concat (getenv "HOME") "/.nix-profile/bin"))
           (systempath "/run/current-system/sw/bin"))
          (setenv "PATH" (concat homepath ":" systempath ":" (getenv "PATH")))
          (add-to-list 'exec-path systempath)
          (add-to-list 'exec-path homepath)
          )

      (setenv "NIX_REMOTE" "daemon")



      (load-theme 'gruvbox)
    '';

    home.file.".emacs.d/custom.el".text = ''
      (custom-set-variables
       ;; custom-set-variables was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(company-idle-delay 0.1)
       '(company-minimum-prefix-length 1)
       '(custom-safe-themes
         (quote
          ("d411730c6ed8440b4a2b92948d997c4b71332acf9bb13b31e9445da16445fe43" "4eb982b248bf818a72877ecb126a2f95d71eea24680022789b14c3dec7629c1b" default)))
       '(global-linum-mode t)
       '(haskell-stylish-on-save t)
       '(haskell-tags-on-save t)
       '(ido-enable-flex-matching t)
       '(ido-everywhere t)
       '(ido-mode (quote both) nil (ido))
       '(idris-interpreter-flags (quote ("-p" "Bi" "-p" "lightyear")))
       '(menu-bar-mode nil)
       '(scroll-bar-mode nil)
       '(tool-bar-mode nil))
      (custom-set-faces
       ;; custom-set-faces was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(default ((t (:inherit nil :stipple nil :background "#282828" :foreground "#fdf4c1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 108 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
       '(idris-definition-face ((t (:inherit font-lock-function-face))))
       '(idris-semantic-bound-face ((t (:foreground "#8ec07c"))))
       '(idris-semantic-data-face ((t (:foreground "#d75f5f"))))
       '(idris-semantic-function-face ((t (:inherit font-lock-function-name-face))))
       '(idris-semantic-type-face ((t (:inherit font-lock-type-face)))))
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

        haskell-mode
        ghc-mod
        hindent

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
