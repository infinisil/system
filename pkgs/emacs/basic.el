; better-defaults neotree gruvbox-theme

(require 'better-defaults)

(require 'recentf)
(recentf-mode)

(let
    ((homepath (concat (getenv "HOME") "/.nix-profile/bin"))
     (systempath "/run/current-system/sw/bin"))
    (setenv "PATH" (concat homepath ":" systempath ":" (getenv "PATH")))
    (add-to-list 'exec-path systempath)
    (add-to-list 'exec-path homepath)
    )

(setenv "NIX_REMOTE" "daemon")

(load-theme 'gruvbox)
