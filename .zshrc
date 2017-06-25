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

POWERLEVEL9K_MODE='nerdfont-fontconfig'
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(root_indicator context dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status command_execution_time time battery)

POWERLEVEL9K_SHORTEN_DIR_LENGTH=1
POWERLEVEL9K_SHORTEN_DELIMITER=""
POWERLEVEL9K_SHORTEN_STRATEGY="truncate_from_right"

plugins=(git pass brew colored-man colorize chucknorris pip python brew osx zsh-autosuggestions zsh-syntax-highlighting)

setopt correctall
setopt COMPLETE_ALIASES

export UPDATE_ZSH_DAYS=30

ENABLE_CORRECTION="true"
HIST_STAMPS="yyyy-mm-dd"

export EDITOR='nvim'
source $ZSH/oh-my-zsh.sh

# Simple function to enumerate all snapshots of a directory
# Example: To list all files of all snapshots of the `dir` directory of the current folder:
# ls $(snaps dir)
#
# To view all versions of a file in vim:
# vim $(snaps dir)
function snaps() {
	local mount=$(stat -c '%m' .)
	echo "$mount/.zfs/snapshot/*/$(realpath . --relative-to=$mount)/$1"
}

function sayrepl() {
	read -r input
	while [ "$input" != "" ];
	do
		say "$input"
		read -r input
	done
}

function rebuild() {
	( cd /global/nixpkgs && git checkout nixos-17.03 && sudo nixos-rebuild switch -I nixpkgs=/global/nixpkgs )
}

alias ethssh='ssh msilvan@slab1.ethz.ch'
alias infssh='ssh root@infinisil.io'
alias vim=nvim
alias vimrc="nvim $XDG_CONFIG_HOME/nvim/init.vim"
alias zshrc="nvim $XDG_CONFIG_HOME/zsh/.zshrc"
alias zshenv="nvim $XDG_CONFIG_HOME/zsh/.zshenv"
alias nixrc="nvim /global/system/nixos"
alias testconfig="sudo nixos-rebuild build -I nixpkgs=/global/nixpkgs -I nixos-config=/global/testconfig.nix"
