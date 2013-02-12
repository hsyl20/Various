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
export PAGER=vimpager
export PRINTER=print-b228

export OCL_ICD_VENDORS=~/.icds

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
#setopt share_history

setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_space

setopt   notify correct pushdtohome cdablevars autolist
setopt   correctall autocd longlistjobs
setopt   autoresume histignoredups pushdsilent noclobber
setopt   autopushd pushdminus extendedglob rcquotes mailwarning
setopt chaselinks
setopt histverify
unsetopt bgnice autoparamslash

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_STYLES[globbing]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[path]='bold'

source ~/.zsh/git-prompt/zshrc.sh

zstyle ':completion:*:vim:*' ignored-patterns '*.(o|a|so|aux|dvi|log|swp|fig|bbl|blg|bst|idx|ind|out|toc|class|pdf|ps)'

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

setopt printeightbit

. ~/.zshprompt

# Automatically set X Property for the current working directory
function chpwd() {
   wid=`xdotool getwindowfocus`
   `xprop -id $wid -f _XMONAD_WORKING_DIR 8s -set _XMONAD_WORKING_DIR $PWD`
}

chpwd

DIRSTACKSIZE=20

# Setup new style completion system. To see examples of the old style (compctl
# based) programmable completion, check Misc/compctl-examples in the zsh
# distribution.
autoload -U compinit
compinit

# Completion Styles

# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'
    
# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
/usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' file-sort access

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# offer indexes before parameters in subscripts
#zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# command for process lists, the local web server details and host completion
#zstyle ':completion:*:processes' command 'ps -o pid,s,nice,stime,args'
#zstyle ':completion:*:urls' local 'www' '/var/www/htdocs' 'public_html'

# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.c~' \
    '*?.old' '*?.pro'
# the same for old style completion
#fignore=(.o .c~ .old .pro)

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'

zstyle ':completion:*:killall:*' command 'ps -u $USER -o cmd'
zstyle ':completion:*:pkill:*' command 'ps -u $USER -o cmd'
compdef pkill=kill
compdef pkill=killall
zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=36=31"

# Ignore parent directory
zstyle ':completion:*:(cd|mv|cp):*' ignore-parents parent pwd

# Ignore what's already in the line
zstyle ':completion:*:(rm|kill|diff|cp|mv):*' ignore-line yes

## add colors to completions
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always
