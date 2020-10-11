{ lib, config, pkgs, ... }:

with lib;

mkIf config.mine.console.enable {

  programs.zsh.promptInit = "source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme";

  mine.userConfig = {

    home.packages = with pkgs; [
      wdiff
      mawk
      colormake
      colordiff
      pythonPackages.pygments
      zsh-completions
      bat
    ];

    home.sessionVariables.EDITOR = "vim";

    programs.zsh = {
      enable = true;
      dotDir = ".config/zsh";
      history.size = 1000000;
      shellAliases = let
      in {
        gist = "${pkgs.gist}/bin/gist";

        ne = "nix-instantiate --eval";
        ni = "nix-instantiate";
        ns = "nix-shell";
        nb = "nix-build";
        feh = "feh -.ZB black";

        gh = "git log --no-merges --date-order --pretty=format:\"%C(auto)%h %s %Cgreen(%cr)\" | fzf +s --preview=\"git show \\$(echo {} | cut -d\\\" \\\" -f1) --color=always\"";

        g = "git";
        ga = "git add";
        gaa = "git add --all";

        gbs = "git bisect";
        gbss = "git bisect start";
        gbsb = "git bisect bad";
        gbsg = "git bisect good";
        gbsr = "git bisect run";
        gbsre = "git bisect reset";

        gc = "git commit -v";
        "gc!" = "git commit -v --amend";
        "gcan!" = "git commit -v -a --no-edit --amend";
        gcb = "git checkout -b";

        gcm = "git checkout master";
        gcmsg = "git commit -m";
        gcp = "git cherry-pick";

        gd = "git diff";
        gdc = "git diff --cached";
        gdw = "git diff --word-diff";
        gdcw = "git diff --cached --word-diff";

        gf = "git fetch";
        gl = "git pull";
        glg = "git log --stat";
        glgp = "git log --stat -p";
        gp = "git push";
        gpsup = "git push -u origin $(git symbolic-ref --short HEAD)";
        gr = "git remote -v";
        grb = "git rebase";
        grbi = "git rebase -i";
        grhh = "git reset --hard HEAD";
        gst = "git status";
        gsts = "git stash show --text";
        gsta = "git stash save";
        gstaa = "git stash apply";
        gstl = "git stash list";
        gstp = "git stash pop";
        glum = "git pull upstream master";
        gwch = "git log --patch --no-merges";

        nix-shell = "nix-shell --command zsh";
      };
      plugins = [
        {
          name = "pass";
          src = "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/pass";
        }
      ];

      initExtra = ''

        source ${./p10k.zsh}
        typeset -g POWERLEVEL9K_CONFIG_FILE=${toString ./p10k.zsh}

        export BAT_CONFIG_PATH=${pkgs.writeText "bat.config" ''
          --theme="Monokai Extended"
        ''}
        alias cat=bat
        alias icat="kitty +kitten icat"

        pst() {
          ssh protos pst $1 | xclip -selection clipboard
        }

        source ${pkgs.fzf}/share/fzf/completion.zsh
        source ${pkgs.fzf}/share/fzf/key-bindings.zsh
        eval "$(${pkgs.direnv}/bin/direnv hook zsh)"

        export HISTFILE=$HOME/.config/zsh/.zsh_history
        export HISTSIZE=1000000
        export SAVEHIST=$HISTSIZE

        export EDITOR=vim

        alias exa="${pkgs.exa}/bin/exa --group-directories-first --color-scale -g"
        alias ls="exa"
        alias l="exa -lh"
        alias t="exa -laTh"

        function mktest() {
          mkdir -p "$HOME/test/$1"
          cd "$HOME/test/$1"
        }

        function pr() {
          ${pkgs.git}/bin/git fetch -fu \
            ''${2:-$(${pkgs.git}/bin/git remote | grep "^upstream" || echo origin)} \
            "refs/pull/$1/head:pr/$1" && \
            ${pkgs.git}/bin/git checkout "pr/$1";
        }

        bindkey '^ ' autosuggest-execute

        unsetopt correct_all
        __cda() {
          local sel

          sel="$(fd -t d -d 5 -H -L | fzf \
            -1 -0 +s +m \
            --height 50% \
            --reverse --border \
            --margin=1 --inline-info \
            --print-query \
            --color=16 \
            --preview="${pkgs.tree}/bin/tree -C {}/ | head -100" \
            | tail -1)"

          if [ -z "$sel" ]; then
            zle redisplay
            return 1
          fi

          mkdir -p "$sel"
          BUFFER="$(printf "cd %q" "$sel")"
          zle accept-line
        }

        __vima() {
          local sel

          sel="$(fd -t f -d 5 -H -L | fzf \
            -m -1 -0 +s \
            --height 50% \
            --reverse --border \
            --margin=1 --inline-info \
            --print-query \
            --color=16 \
            --preview="${pkgs.highlight}/bin/highlight -O ansi --force {}" \
            | tail -1)"

          if [ -z "$sel" ]; then
            zle redisplay
            return 1
          fi

          BUFFER="$(printf "vim %q" "$sel")"
          zle accept-line
        }

        _mpc_sendmessage() {
          compadd $(mpc channels)
        }
        _mpc_subscribe() {
          compadd $(mpc channels)
        }
        _mpc_waitmessage() {
          compadd $(mpc channels)
        }

        pull() {
          url=$(curl "https://api.github.com/repos/NixOS/nixpkgs/pulls/$1" \
            | jq -r '.head | .repo.html_url + "/archive/" + .sha + ".tar.gz"')
          nix-prefetch-url --print-path --unpack "$url" | tail -1
        }

        zle -N __cda
        zle -N __vima

        chpwd() l;

        bindkey '^A^G' __vima
        bindkey '^A^H' __cda

        bindkey -M vicmd v edit-command-line
      '';
    };

  };
}
