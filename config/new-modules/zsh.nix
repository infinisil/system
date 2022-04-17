{ lib, config, pkgs, ... }:

with lib;

mkIf config.mine.console.enable {

  programs.zsh = {
    enable = true;
    histSize = 1000000;
    syntaxHighlighting.enable = true;
    autosuggestions.enable = true;
    enableBashCompletion = true;
    promptInit = "source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme";

    interactiveShellInit = ''
      source ${./p10k.zsh}
      typeset -g POWERLEVEL9K_CONFIG_FILE=${toString ./p10k.zsh}

      export BAT_CONFIG_PATH=${pkgs.writeText "bat.config" ''
        --theme="Monokai Extended"
        --plain
      ''}

      pst() {
        ssh protos pst $1 | xclip -selection clipboard
      }

      source ${pkgs.fzf}/share/fzf/completion.zsh
      source ${pkgs.fzf}/share/fzf/key-bindings.zsh
      eval "$(${pkgs.direnv}/bin/direnv hook zsh)"

      function mktest() {
        mkdir -p "$HOME/test/$1"
        cd "$HOME/test/$1"
      }

      bindkey '^ ' autosuggest-execute

      __cda() {
        local sel

        sel="$(fd -t d -H -L | fzf \
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

        sel="$(fd -t f -H -L | fzf \
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

      zle -N __cda
      zle -N __vima

      chpwd() {
        eval l
      }

      export LESS_TERMCAP_mb=$'\E[01;31m'      # Begins blinking.
      export LESS_TERMCAP_md=$'\E[01;31m'      # Begins bold.
      export LESS_TERMCAP_me=$'\E[0m'          # Ends mode.
      export LESS_TERMCAP_se=$'\E[0m'          # Ends standout-mode.
      export LESS_TERMCAP_so=$'\E[00;47;30m'   # Begins standout-mode.
      export LESS_TERMCAP_ue=$'\E[0m'          # Ends underline.
      export LESS_TERMCAP_us=$'\E[01;32m'      # Begins underline.

      bindkey '^A^G' __vima
      bindkey '^A^H' __cda

      autoload -Uz edit-command-line
      zle -N edit-command-line
      bindkey -M vicmd v edit-command-line
    '';
  };

  environment.shellAliases = {
    exa = "exa --group-directories-first --color-scale -g";
    ls = "exa";
    l = "exa -lh";
    t = "exa -laTh";
    cat = "bat";
    icat = "kitty +kitten icat";
    nix-shell = "nix-shell --command zsh";
    feh = "feh -.ZB black";
    ne = "nix-instantiate --eval";
    ni = "nix-instantiate";
    ns = "nix-shell";
    nb = "nix-build";
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
    gsts = "git stash show --text --include-untracked";
    gsta = "git stash save";
    gstaa = "git stash apply";
    gstl = "git stash list";
    gstp = "git stash pop";
    glum = "git pull upstream master";
    gwch = "git log --patch --no-merges";
  };
}
