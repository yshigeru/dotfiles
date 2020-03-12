# -*- shell-script -*-

set_prompt_color()
{
    if [ $? -eq 0 ]; then
        tput setaf 2		# green for command success
    else
        tput setaf 1		# red for command failure
    fi
    tput bold
}

reset_prompt_color()
{
    tput sgr0
}

prompt_command()
{
    # After each command, append to the history file and reread it
    history -a
    history -c
    history -r
}

#PROMPT_DIRTRIM=2
#PS1='\[$(set_prompt_color)\]\h:\w\\$ \[$(reset_prompt_color)\]'
PS1='\[$(set_prompt_color)\]\h:$(echo "\w" | sed -e "/^.\{30,\}/s/^.*\(.\{28\}\)/..\1/") \\$ \[$(reset_prompt_color)\]'
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}prompt_command"

HISTCONTROL=ignoredups:erasedups # Avoid duplicates
HISTSIZE=1000000

shopt -s globstar
shopt -s checkwinsize
shopt -s histappend # When the shell exits, append to the history file instead of overwriting it

export EDITOR=vi
export PAGER=less
export TERM=xterm-256color

alias j=jobs
alias h='history 20'
alias ls='ls --color=auto'
alias ll='ls -lh'
alias la='ls -a'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias e='emacsclient -n'
# alias e='with_tmux_rename_window e emacsclient -t -a ""'
# alias mew='with_tmux_rename_window mew emacsclient -e "(mew)" -t -a ""'
# alias start-emacs='emacs --daemon'
# alias kill-emacs='emacsclient -e "(kill-emacs)"'
alias p='pwd | sed "s,^$HOME,~,"'
alias pd=peco-dirs-cd
alias fd=peco-find-cd
alias pk=peco-pkill

cd()
{
    if [ -z "$1" ] ; then
        test "$PWD" != "$HOME" && pushd $HOME > /dev/null
	return 0
    elif ( echo "$1" | egrep "^\.\.\.+$" > /dev/null ) ; then
        cd $( echo "$1" | perl -ne 'print "../" x ( tr/\./\./ - 1 )' )
    else
        pushd "$1" > /dev/null
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

with_tmux_rename_window()
{
    local win_name=$1
    shift

    if [ "$TMUX" != "" ]; then
	local old_win_name=`tmux display-message -p '#W'`

	tmux rename-window $win_name
	command "$@"
	tmux set-window-option automatic-rename "on" 1>/dev/null
	tmux rename-window $old_win_name
    else
	command "$@"
    fi
}

# enable bash completion in interactive shells
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
    fi
fi

umask 022
