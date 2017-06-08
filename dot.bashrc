# -*- shell-script -*-
PS1="\[\033[32m\]jobs:\j [\w]\[\033[0m\]\n\`if [ \$? = 0 ]; then echo '\[\033[1;36m\]'; else echo '\[\033[1;31m\]'; fi\`\u@\H\[\033[1;33m\]\$ \[\033[0m\]"
#PS1="\`if [ \$? = 0 ]; then echo '\[\033[1;36m\]'; else echo '\[\033[1;31m\]'; fi\`\u@\H\[\033[1;33m\]\$ \[\033[0m\]"
HISTSIZE=1000000

export EDITOR=vi
export PAGER=less

alias j=jobs
alias h=history
alias ls='ls --color=auto'
alias ll='ls -lh'
alias la='ls -a'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias ec='emacsclient -n'
