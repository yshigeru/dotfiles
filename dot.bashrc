# -*- shell-script -*-
export PATH=$HOME/bin:$PATH:/sbin:/usr/sbin

if [ "$EMACS" != "" ]; then
   export EDITOR=emacsclient
   #export PAGER=emacsclient
   LANG=C
else
   export EDITOR=vi
   export PAGER=less
fi   

alias j=jobs
alias h=history
alias ls='ls --color=auto'
alias ll='ls -lh'
alias la='ls -a'
alias grep='grep --color=always'
alias egrep='egrep --color=always'
alias fgrep='fgrep --color=always'
alias etags=/usr/bin/etags
alias ec=emacsclient
