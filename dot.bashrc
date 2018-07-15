# -*- shell-script -*-
right_prompt()
{
    local prompt len half_columns start

    tput sc
    prompt=`pwd | sed "s|^$HOME|~|"`

    len=${#prompt}
    half_columns=$((COLUMNS / 2))

    if [ $len -gt $half_columns ]; then
	start=$((len - half_columns + 3))
	prompt="...${prompt:start:len}"
    fi

    printf '%*s' $COLUMNS $prompt
    tput rc
}

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

PS1='\[$(set_prompt_color; right_prompt)\]\u@\h \\$ \[$(reset_prompt_color)\]'

# Avoid duplicates
HISTCONTROL=ignoredups:erasedups  
# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend
# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

HISTSIZE=1000000

shopt -s globstar
shopt -s checkwinsize

export EDITOR=vi
export PAGER=less
export TERM=screen-256color

alias j=jobs
alias h='history 20'
alias ls='ls --color=auto'
alias ll='ls -lh'
alias la='ls -a'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias e='emacsclient -n'

# enable bash completion in interactive shells
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
    fi
fi
