# Path to your oh-my-zsh installation.
export ZSH=$ZDOTDIR/oh-my-zsh

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

autoload -Uz promptinit
promptinit
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=$ZDOTDIR/.zsh_history

autoload -Uz compinit
compinit

ZSH_THEME="powerlevel9k/powerlevel9k"
DEFAULT_USER=infinisil

POWERLEVEL9K_MODE='nerdfont-complete'
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(os_icon root_indicator context dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status command_execution_time time battery)

plugins=(git pass brew colored-man colorize chucknorris pip python brew osx zsh-autosuggestions zsh-syntax-highlighting)

setopt correctall
setopt COMPLETE_ALIASES

export UPDATE_ZSH_DAYS=30

ENABLE_CORRECTION="true"

HIST_STAMPS="yyyy-mm-dd"

# User configuration
#export IPFS_PATH="$XDG_DATA_HOME/ipfs"
export EDITOR='nvim'
source $ZSH/oh-my-zsh.sh

#alias ipfs="ipfs --config=$XDG_CONFIG_HOME/ipfs/config"
alias ethssh='ssh msilvan@slab1.ethz.ch'
alias infssh='ssh infinisil@infinisil.io'
alias vim=nvim
alias vimrc="nvim $XDG_CONFIG_HOME/nvim/init.vim"
alias zshrc="nvim $XDG_CONFIG_HOME/zsh/.zshrc"
alias zshenv="nvim $XDG_CONFIG_HOME/zsh/.zshenv"
alias nixrc="nvim $SYSTEM/nixos/mac-configuration.nix"
alias emacs="emacseditor -c &"
alias rebuild="sudo nixos-rebuild switch"
