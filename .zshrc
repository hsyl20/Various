# Use hard limits, except for a smaller stack and no core dumps
unlimit
limit stack 8192
limit core 0
limit -s

# Aliases
alias s="cd .."
alias df="df -h"
alias ls="ls --color"
alias ll="ls -lh"
alias la="ls -a"
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
alias grep="grep --color --exclude-dir=.svn --exclude=Makefile.in -I"

# Exports
export XAUTHORITY="$HOME/.Xauthority"

export PATH=$PATH:~/.cabal/bin:~/.usr/bin
export LD_LIBRARY_PATH=/home/shenry/.usr/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/home/shenry/.usr/lib/pkgconfig:/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH

export EDITOR=vim
export PRINTER=print-b228

########################
# ZSH 
########################
autoload -U compinit promptinit
compinit
promptinit
 
setopt completealiases

setopt prompt_subst

export HISTFILE=~/.history
export HISTSIZE=1000
export SAVEHIST=1000

# History commands are appended to the existing file instead of
# overwriting it.
setopt append_history
# Sequential duplicate commands only get one history entry.
setopt hist_ignore_all_dups
# Only the newest of a set of duplicates (regardless of sequence) is saved
# to file.
setopt hist_save_no_dups
# Ignore les lignes commencant par un espace
setopt hist_ignore_space

# Commands are added to the history file as they are entered.
setopt inc_append_history
# setopt share_history

source ~/.zsh/git-prompt/zshrc.sh
PROMPT='[%m:%~]%# '    # default prompt
RPROMPT=' $(git_super_status)'     # prompt for right side of screen

zstyle ':completion:*:vim:*' ignored-patterns '*.(o|a|so|aux|dvi|log|swp|fig|bbl|blg|bst|idx|ind|out|toc|class|pdf|ps)'

bindkey "^[[5~" history-search-backward # PageUp
bindkey "^[[6~" history-search-forward # PageDown
bindkey "^[[1~" beginning-of-line # Home (console)
bindkey "^[[H"  beginning-of-line # Home (xterm)
bindkey "^[[4~" end-of-line # End (console)
bindkey "^[[F"  end-of-line # End (xterm)
bindkey "^[."  insert-last-word

. ~/.zshprompt
