{ config, pkgs, ... }:

let

  prezto = pkgs.fetchFromGitHub {
    owner = "sorin-ionescu";
    repo = "prezto";
    rev = "4b0ecffacadec6c9553be894cdcd36ecafac7662";
    sha256 = "1dzfd0hwchmwgd61386dk2q813nvbqfbynn3r56w4965s9923mk3";
    fetchSubmodules = true;
    postFetch = ''
      cd $out
      for f in $(find modules -type f -name '*.zsh'); do
        substituteInPlace $f --replace \
          'cache_file="''${0:h}/cache.zsh"' \
          "cache_file=\"\$HOME/.cache/prezto/$(basename $(dirname $f)).zsh\""
      done
    '';
  };
in

{

  home-manager.users.infinisil = {

    home.packages = with pkgs; [
      wdiff
      mawk
      colormake
      colordiff
      pythonPackages.pygments
      zsh-completions
    ];

    home.sessionVariables.EDITOR = "${config.home-manager.users.infinisil.programs.vim.package}/bin/vim";

    home.sessionVariableSetter = "zsh";

    programs.zsh = let
      vim = "${config.home-manager.users.infinisil.programs.vim.package}/bin/vim";
    in {
      enable = true;
      dotDir = ".config/zsh";
      history.size = 100000;
      history.path = ".config/zsh/.zsh_history";
      shellAliases = let
        exa = "${pkgs.exa}/bin/exa --group-directories-first";
      in {
        gist = "${pkgs.gist}/bin/gist -s";
        e = "emacsclient -n -c";
        rh = "home-manager switch";
        sc = "sudo systemctl";
        scu = "systemctl --user";
        jc = "journalctl";
        jcu = "journalctl --user";
        v = "vim";
        f = "vim $(fd | fzf)";
        ne = "nix-instantiate --eval";
        ni = "nix-instantiate";
        ns = "nix-shell";
        nb = "nix-build";
        feh = "feh -.ZB black";

        beet = "noglob beet";

        g = "git";
        ga = "git add";
        gaa = "git add --all";

        gbs = "git bisect";
        gbsb = "git bisect bad";
        gbsg = "git bisect good";
        gbsr = "git bisect reset";
        gbss = "git bisect start";

        gc = "git commit -v";
        "gc!" = "git commit -v --amend";
        "gcan!" = "git commit -v -a --no-edit --amend";
        gcb = "git checkout -b";

        gcm = "git checkout master";
        gcmsg = "git commit -m";
        gcp = "git cherry-pick";

        gd = "git diff";
        gdca = "git diff --cached";
        gdw = "git diff --word-diff";
        gdcw = "git diff --cached --word-diff";

        gf = "git fetch";
        gl = "git pull";
        glg = "git log --stat";
        glgp = "git log --stat -p";
        gp = "git push";
        gr = "git remote";
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
        gwch = "git whatchanged -p --abbrev-commit --pretty=medium";

        # This makes it so that every library is included by default
        idris = ''idris $($(which -p idris) --listlibs | grep -v ".*\.ibc$" | sed -e "s/^/-p /" | paste -sd" ")'';
      };
      initExtra = ''
        HISTFILE=$HOME/.config/zsh/.zsh_history

        function mktest() {
          mkdir -p $HOME/Test/$1
          cd $HOME/Test/$1
        }

        function nixrc() {
          cd /cfg
          ${vim} .
          git commit -av
        }

        function homerc() {
          cd $HOME/cfg
          ${vim} .
          git commit -av
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
          cd ~/src/nixpkgs
          ${pkgs.git}/bin/git fetch -fu \
            ''${2:-$(${pkgs.git}/bin/git remote | grep "^upstream" || echo origin)} \
            "refs/pull/$1/head:pr/$1" && \
            ${pkgs.git}/bin/git checkout "pr/$1";
        }

        mkdir -p $HOME/.cache/prezto

        source ${prezto}/init.zsh
        source ${pkgs.fzf}/share/fzf/completion.zsh
        source ${pkgs.fzf}/share/fzf/key-bindings.zsh

        bindkey '^ ' autosuggest-execute

        unalias -m 'l*'
        alias ls=exa

        alias a='fasd -a'        # any
        alias s='fasd -si'       # show / search / select
        alias d='fasd -d'        # directory
        alias f='fasd -f'        # file
        alias sd='fasd -sid'     # interactive directory selection
        alias sf='fasd -sif'     # interactive file selection
        alias z='fasd_cd -d'     # cd, same functionality as j in autojump
        alias zz='fasd_cd -d -i' # cd with interactive selection

        alias v='f -e vim'
        alias o='a -e xdg-open'

        alias exa='exa --group-directories-first --color-scale -g'
        alias l='exa -laah'
        alias t='exa -laTh'

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
        'git' \
        'fasd' \
        'syntax-highlighting' \
        'history-substring-search' \
        'autosuggestions' \
        'prompt'

      zstyle ':prezto:module:editor' key-bindings 'vi'
      zstyle ':prezto:module:prompt' theme 'sorin'
      zstyle ':prezto:module:utility' safe-ops 'no'.
    '';
  };
}
