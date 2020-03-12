# -*- shell-script -*-
# Set up the prompt
#PROMPT='%(?.%K{blue}.%K{red})%n@%m%k %F{white}%# %b%f%k'
#RPROMPT='%B%F{green}%53<...<%~%}'
PROMPT='%(?.%B%F{green}.%B%F{red})%m%k %# %F{white}%b%f%k'
RPROMPT='%(?.%B%F{green}.%B%F{red})%53<...<%~%}'

setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000000
SAVEHIST=1000000
HISTFILE=~/.zsh_history
setopt hist_ignore_dups
setopt share_history

# Use directory stack
DIRSTACKSIZE=100
setopt AUTO_PUSHD
autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*:descriptions' format '%BCompleting%b %U%d%u'

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# 
setopt nolistbeep
# autoload predict-on
# predict-on

alias j=jobs
alias h=history
alias ls='ls --color=auto'
alias ll='ls -lh'
alias la='ls -a'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
# alias e='emacsclient -n'
alias e='emacsclient -t -a ""'
alias ec='emacsclient -n'
alias mew='emacsclient -e "(mew)" -t -a ""'
alias start-emacs='emacs --daemon'
alias kill-emacs='emacsclient -e "(kill-emacs)"'
alias p='pwd | sed "s,^$HOME,~,"'
# alias pd=peco-dirs-cd
# alias fd=peco-find-cd
# alias pk=peco-pkill

EDITOR=vi
PAGER=less
TERM=xterm-256color

peco-history-selection() {
    BUFFER=`history -n 1 | tac  | awk '!a[$0]++' | peco`
    CURSOR=$#BUFFER
    zle reset-prompt
}

zle -N peco-history-selection
bindkey '^R' peco-history-selection

# cdr
if [[ -n $(echo ${^fpath}/chpwd_recent_dirs(N)) && -n $(echo ${^fpath}/cdr(N)) ]]; then
    autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
    add-zsh-hook chpwd chpwd_recent_dirs
    zstyle ':completion:*' recent-dirs-insert both
    zstyle ':chpwd:*' recent-dirs-default true
    zstyle ':chpwd:*' recent-dirs-max 1000
    zstyle ':chpwd:*' recent-dirs-file "$HOME/.cache/chpwd-recent-dirs"
fi

function peco-cdr () {
    local selected_dir="$(cdr -l | sed 's/^[0-9]\+ \+//' | peco --prompt="cdr >" --query "$LBUFFER")"
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
}

zle -N peco-cdr
bindkey '^W' peco-cdr

# Start Dropbox
if [ -e /usr/bin/dropbox ] && dropbox running; then
    daemon dropbox start
fi

umask 022
