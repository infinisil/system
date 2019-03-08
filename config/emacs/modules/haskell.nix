{ pkgs, config, lib, epkgs, dag, ... }:


with lib;

let

  hie = import (import ../../sources).hie-nix {};

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
      (require 'lsp)
      (require 'lsp-haskell)

      (defun hie-wrapper (argv)
        (append
          (append (list "nix-shell" "--pure" "--command")
                  (list (mapconcat 'identity argv " ")))
          ; FIXME: Doesn't work with only a default.nix
          (list (concat (lsp-haskell--get-root) "/shell.nix"))))

      (defun hasky-keys ()
        "Hasky extension key binds"
        (require 'hasky-extensions)
        (local-set-key (kbd "C-c C-y") #'hasky-extensions))

      (setq lsp-haskell-process-path-hie "${hie.hies}/bin/hie-wrapper")
      (setq lsp-haskell-process-args-hie (quote ("--vomit" "-d" "-l" "/tmp/hie.log")))
      (setq lsp-haskell-process-wrapper-function 'hie-wrapper)

      (setq lsp-auto-guess-root t)
      (setq lsp-prefer-flymake nil)
      (setq lsp-ui-doc-max-height 10)
      (setq lsp-ui-doc-max-width 80)
      (setq lsp-ui-sideline-ignore-duplicate t)

      (add-hook 'haskell-mode-hook #'lsp)
      (add-hook 'haskell-mode-hook (lambda () (haskell-indentation-mode nil)))
      (add-hook 'haskell-mode-hook 'hasky-keys)
    '';
  };
}
