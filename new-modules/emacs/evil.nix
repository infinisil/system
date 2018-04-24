{ pkgs, lib, config, ... }:

with lib;

{

  mine.emacs.config = { epkgs, dag, ... }: {

    packages = with epkgs; [
      evil
      evil-collection
      evil-leader
      which-key
    ];

    init = {

      evil = dag.entryAfter [ "base" ] ''
        (setq evil-want-integration nil)
        (evil-collection-init)
        (setq evil-collection-setup-minibuffer t)
        (evil-mode 1)

        (global-evil-leader-mode)
        (require 'evil-leader)

        (evil-leader/set-leader "<SPC>")

        (setq evil-leader/in-all-states t)
      '';

      keys = dag.entryAfter [ "evil" ] ''
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

        (global-set-key (kbd "M-p") (defun revert-buffer-no-confirm() (interactive) (revert-buffer t t)))

        (which-key-add-key-based-replacements "SPC w" "window")
        (evil-leader/set-key "w o" 'delete-other-windows)
        (evil-leader/set-key "w k" 'delete-windows)
      '';

    };
  };
}

