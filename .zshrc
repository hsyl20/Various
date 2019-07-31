# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="robbyrussell"
ZSH_THEME="wedisagree"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
PROMPT='%{$fg[magenta]%}[%m:%~] %{$reset_color%}'

unsetopt share_history
unsetopt beep

# aliases
alias v="gvim"
alias s="cd .."
alias df="df -h"
alias ls="ls --color"
alias ll="ls -lh"
alias la="ls -a"
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
alias grep="grep --color --exclude-dir=.svn --exclude=Makefile.in --exclude=\\*.html --exclude=\\*.tmp2 --exclude=\\*.tmp -I"
alias ag="ag --ignore-dir=.svn --ignore=Makefile.in --ignore=\\*.html --ignore=\\*.tmp2 --ignore=\\*.tmp"
alias git="LC_ALL=C git"
alias todo="vim ~/travail/haskus/todo"

alias streamlink-live="streamlink"
alias streamlink-replay="streamlink --player-passthrough=hls --twitch-oauth-token=7hgig7wvv31nif4h0k2fex3stz30mz"

# Exports
export XAUTHORITY="$HOME/.Xauthority"

export PATH=~/.cabal/bin:/home/hsyl20/.usr/bin:/home/hsyl20/.local/bin:$PATH
export LD_LIBRARY_PATH=/home/hsyl20/.usr/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/home/hsyl20/.usr/lib/pkgconfig:/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH

export EDITOR=vim
export PAGER=less
export _JAVA_AWT_WM_NONREPARENTING=1

bindkey "^[[5~" history-beginning-search-backward # PageUp
bindkey "^[[6~" history-beginning-search-forward # PageDown
bindkey "^[[1~" beginning-of-line # Home (console)
bindkey "^[[H"  beginning-of-line # Home (xterm)
bindkey "^[[4~" end-of-line # End (console)
bindkey "^[[F"  end-of-line # End (xterm)
bindkey "^[."  insert-last-word
bindkey "^H" backward-delete-char
bindkey "^[[3~" delete-char
bindkey "^[[1;5D" backward-word 
bindkey "^[[1;5C" forward-word 

autoload -Uz compinit
compinit
# Completion for kitty
kitty + complete setup zsh | source /dev/stdin

# Automatically set X Property for the current working directory
function chpwd() {
   if xset q &>/dev/null; then
      wid=`xdotool getwindowfocus`
      `xprop -id $wid -f _XMONAD_WORKING_DIR 8s -set _XMONAD_WORKING_DIR $PWD`
   fi
}

chpwd

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn
precmd() {
    vcs_info
}
