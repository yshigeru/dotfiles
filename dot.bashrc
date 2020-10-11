# -*- shell-script -*-

umask 022

shopt -s globstar
shopt -s checkwinsize

# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend

PROMPT_DIRTRIM=2
#PS1='\[$(set_prompt_color)\]\h:\w\\$ \[$(reset_prompt_color)\]'
#PS1='\[$(set_prompt_color)\]\h:$(echo "\w" | sed -e "/^.\{30,\}/s/^.*\(.\{28\}\)/..\1/") \\$ \[$(reset_prompt_color)\]'
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}prompt_command"

HISTCONTROL=ignoredups:erasedups # Avoid duplicates
HISTSIZE=1000000

export EDITOR=vim
export PAGER=less

case "$TERM" in
    rxvt-unicode-256color) ;;
    eterm-color) ;;
    *) export TERM=xterm-256color
esac

prompt_command()
{
    # Set prompt color depending on exit code of the last command
    local ex=$?
    local color='\e[1;32m' # green
    local reset='\e[0m'

    [ "$ex" -ne 0 ] && color=$'\e[1;31m' # red
    PS1="\[$color\]\u@\h:\w \$ \[$reset\]"

    # After each command, append to the history file and reread it
    history -a
    history -c
    history -r
}

cd()
{
    if [ -z "$1" ] ; then
        test "$PWD" != "$HOME" && pushd $HOME > /dev/null
	return 0
    elif ( echo "$1" | egrep "^\.\.\.+$" > /dev/null ); then
        cd $( echo "$1" | perl -ne 'print "../" x ( tr/\./\./ - 1 )' )
    else
        pushd "$1" > /dev/null
    fi
}

cdh()
{
    local dirnum

    dirs -v | sort -k 2 | uniq -f 1 | sort -n -k 1 | head -n $(( LINES - 3 ))
    read -p "select number: " dirnum

    if [ -z "$dirnum" ]; then
        echo "$FUNCNAME: Abort." 1>&2
    elif ( echo $dirnum | egrep '^[[:digit:]]+$' > /dev/null ); then
        cd "$( echo ${DIRSTACK[$dirnum]} | sed -e "s;^~;$HOME;" )"
    else
        echo "$FUNCNAME: Wrong." 1>&2
    fi
}

peco-dirs-cd()
{
    local newdir=$( dirs -v | sort -k 2 | uniq -f 1 | sort -n -k 1 | awk '{ print $2 }' | peco )
    if [ "$newdir" != "" ]; then
	cd $( echo $newdir | sed -e "s;^~;$HOME;" )
    fi
}

peco-find-cd()
{
    local newdir=$( find . -type d | peco )
    if [ "$newdir" != "" ]; then
	cd $newdir
    fi
}

peco-pkill()
{
    local pid

    for pid in $( ps aux | peco | awk '{ print $2 }' ); do
	kill $pid
	echo "Killed ${pid}"
    done
}

peco-history()
{
    local cmd

    history -d -1
    cmd=$( history | tac  | sed -E 's/^ +[0-9]+ +//' | peco )
    $cmd
}

alias j=jobs
alias h='history 20'
alias ls='ls --color=auto'
alias ll='ls -lh'
alias la='ls -a'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias p='pwd | sed "s,^$HOME,~,"'
alias pd=peco-dirs-cd
alias fd=peco-find-cd
alias pk=peco-pkill
alias ph=peco-history

# alias e='emacsclient -n'
if [ "$TERM" = eterm-color ]; then
    alias e='emacsclient -n'
else
    alias e='emacsclient -t -a ""'
    alias ec='emacsclient -n'
    alias mew='emacsclient -e "(mew)" -t -a ""'
    alias start-emacs='emacs --daemon'
    alias kill-emacs='emacsclient -e "(kill-emacs)"'
fi

# enable bash completion in interactive shells
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
    fi
fi
