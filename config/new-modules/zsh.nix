{ lib, config, pkgs, ... }:

with lib;

let

  prezto = pkgs.stdenv.mkDerivation {
    name = "prezto";
    src = (import ../sources).prezto;

    buildInputs = [ pkgs.zsh ];

    buildPhase = ''
      for f in $(find modules -type f -name '*.zsh'); do
        substituteInPlace "$f" --replace \
          'cache_file="''${0:h}/cache.zsh"' \
          "cache_file=\"\$HOME/.cache/prezto/$(basename $(dirname $f)).zsh\""
      done

      for f in $(find . -type f -name '*.zsh'); do
        zsh -c "zcompile $f"
      done
    '';

    installPhase = ''
      cp -r . $out
    '';
  };

in


mkIf config.mine.console.enable {

  mine.userConfig = {

    home.packages = with pkgs; [
      wdiff
      mawk
      colormake
      colordiff
      pythonPackages.pygments
      zsh-completions
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

        __opt_repeat_last() {
          [[ -n $BUFFER ]] || zle up-history
          zle accept-line
        }

        pst() {
          ssh protos pst $1 | xclip -selection clipboard
        }

        zle -N repeat_last __opt_repeat_last

        bindkey '^M' repeat_last

        source ${prezto}/init.zsh
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

        function nsrc() {
          set -euo pipefail
          if [ -z "$1" ]; then
            echo Give a nixpkgs attribute to check the source of
            return 1
          fi
          cd /tmp && mkdir -p "$1" && cd "$1"
          nix-shell "''${2:-<nixpkgs>}" -Q -A "$1" --command unpackPhase
          cd *
        }

        function pr() {
          ${pkgs.git}/bin/git fetch -fu \
            ''${2:-$(${pkgs.git}/bin/git remote | grep "^upstream" || echo origin)} \
            "refs/pull/$1/head:pr/$1" && \
            ${pkgs.git}/bin/git checkout "pr/$1";
        }

        mkdir -p $HOME/.cache/prezto

        bindkey '^ ' autosuggest-execute

        unsetopt correct_all
        __cd() {
          local sel

          sel="$(fasd -Rdl | xargs -I{} realpath --relative-base=$PWD -s "{}" | fzf \
            -0 +s +m \
            --height 50% \
            --reverse --border \
            --margin=1 --inline-info \
            --color=16 \
            --print-query \
            --preview="${pkgs.tree}/bin/tree -C {}/ | head -100" \
            | tail -1)"

          if [ -z "$sel" ]; then
            zle redisplay
            return 1
          fi

          BUFFER="$(printf "cd %q" "$sel")"
          zle redisplay
          zle accept-line
        }

        __vim() {
          local sel

          sel="$(fasd -Rfl | xargs -I{} realpath --relative-base=$PWD -s "{}" | fzf \
            -m +s -0 \
            --height 50% \
            --reverse --border \
            --margin=1 --inline-info \
            --color=16 \
            --print-query \
            --preview="${pkgs.highlight}/bin/highlight -O ansi --force {}" \
            | tail -1)"

          if [ -z "$sel" ]; then
            zle redisplay
            return 1
          fi

          BUFFER="$(printf "vim %q" "$sel")"
          #zle redisplay
          zle accept-line
        }

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

        zle -N __cd
        zle -N __vim
        zle -N __cda
        zle -N __vima

        chpwd() l;

        bindkey '^G' __vim
        bindkey '^H' __cd
        bindkey '^A^G' __vima
        bindkey '^A^H' __cda

        bindkey -M vicmd v edit-command-line
      '';
    };

    home.file.".config/zsh/.zlogin".source = "${prezto}/runcoms/zlogin";

    home.file.".config/zsh/.zpreztorc".text = ''
      zstyle ':prezto:*:*' color 'yes'
      zstyle ':prezto:load' pmodule \
        'environment' \
        'terminal' \
        'editor' \
        'history' \
        'directory' \
        'spectrum' \
        'utility' \
        'completion' \
        'fasd' \
        'syntax-highlighting' \
        'history-substring-search' \
        'autosuggestions' \
        'prompt' \
        'git'

      zstyle ':prezto:module:editor' key-bindings 'vi'
      zstyle ':prezto:module:prompt' theme 'sorin'
      zstyle ':prezto:module:utility' safe-ops 'no'.
    '';
  };
}
