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

HISTFILE=~/.history
HISTSIZE=10000
SAVEHIST=10000

setopt extended_history
setopt append_history
setopt inc_append_history
setopt share_history

setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_space



source ~/.zsh/git-prompt/zshrc.sh

zstyle ':completion:*:vim:*' ignored-patterns '*.(o|a|so|aux|dvi|log|swp|fig|bbl|blg|bst|idx|ind|out|toc|class|pdf|ps)'

bindkey "^[[5~" history-beginning-search-backward # PageUp
bindkey "^[[6~" history-beginning-search-forward # PageDown
bindkey "^[[1~" beginning-of-line # Home (console)
bindkey "^[[H"  beginning-of-line # Home (xterm)
bindkey "^[[4~" end-of-line # End (console)
bindkey "^[[F"  end-of-line # End (xterm)
bindkey "^[."  insert-last-word

setopt printeightbit

. ~/.zshprompt
