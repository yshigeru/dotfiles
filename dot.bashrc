# -*- shell-script -*-
PS1="\`if [ \$? = 0 ]; then echo '\[\e[0;44m\]'; else echo '\[\e[0;41m\]'; fi\`\h \\$\[\e[0m\] "

# Avoid duplicates
HISTCONTROL=ignoredups:erasedups  
# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend
# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

HISTSIZE=1000000

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
