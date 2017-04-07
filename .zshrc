# Path to your oh-my-zsh installation.
export ZSH=$ZDOTDIR/oh-my-zsh
export TERM=xterm-256color

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

eval "$(thefuck --alias)"

autoload -Uz promptinit
promptinit
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=$ZDOTDIR/.zsh_history

autoload -Uz compinit
compinit

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="powerlevel9k/powerlevel9k"
DEFAULT_USER=infinisil

POWERLEVEL9K_MODE='awesome-fontconfig'
#POWERLEVEL9K_PROMPT_ADD_NEWLINE=true
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(os_icon root_indicator context dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status command_execution_time time battery)

plugins=(git pass brew colored-man colorize chucknorris pip python brew osx zsh-autosuggestions zsh-syntax-highlighting)

setopt correctall
setopt COMPLETE_ALIASES

export UPDATE_ZSH_DAYS=30

ENABLE_CORRECTION="true"

HIST_STAMPS="yyyy-mm-dd"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# User configuration
export GOPATH="$XDG_CACHE_HOME/go"
export RUST_SRC_PATH=/usr/local/src/rust/src
export IPFS_PATH="$XDG_DATA_HOME/ipfs"
# export PATH="/usr/bin:/bin:/usr/sbin:/sbin:$PATH"
# export MANPATH="/usr/local/man:$MANPATH"

export EDITOR='nvim'

source $ZSH/oh-my-zsh.sh

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#

#alias ipfs="ipfs --config=$XDG_CONFIG_HOME/ipfs/config"
alias start_zeronet="tmux new -d -s zeronet 'cd /Applications/ZeroBundle; ./Python/python ZeroNet/zeronet.py'"
alias start_ipfs="tmux new -d -s ipfs 'ipfs daemon'"
alias ethssh='ssh msilvan@slab1.ethz.ch'
alias infssh='ssh infinisi@infinisil.io -p18765'
alias vim=nvim
alias vimrc="nvim $XDG_CONFIG_HOME/nvim/init.vim"
alias zshrc="nvim $XDG_CONFIG_HOME/zsh/.zshrc"
alias zshenv="nvim $XDG_CONFIG_HOME/zsh/.zshenv"
alias tmux="tmux -f $HOME/.config/tmux/.tmux.conf"
# Example aliases
# alias zshrc="$EDITOR ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
source ~/.profile

