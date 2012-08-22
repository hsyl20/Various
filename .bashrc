# Source global definitions
if [ -f /etc/profile.bash ]; then
	. /etc/profile.bash
fi

export XAUTHORITY="$HOME/.Xauthority"

PATH=$PATH:~/.usr/bin
PATH=$PATH:~/.cabal/bin
export LD_LIBRARY_PATH=/home/shenry/.usr/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/home/shenry/.usr/lib/pkgconfig:/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH

#########################
#Scala
#########################
SCALA_HOME=/opt/scala
export SCALA_HOME
PATH=$PATH:/opt/scala/bin


#########################
# Aliases
#########################
alias s="cd .."
alias df="df -h"
alias ls="ls --color"
alias ll="ls -lh"
alias la="ls -a"
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
alias grep="grep --exclude-dir=.svn --exclude=Makefile.in -I"

export EDITOR=vim
export PATH

source /usr/share/git/completion/git-prompt.sh

export PS1="[\u@\h\$(__git_ps1) \W]\$ "
