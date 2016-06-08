# -*- shell-script -*-
export PATH=/opt/linaro/bin:/opt/powerpc-devel/bin:$HOME/bin:$PATH:/sbin:/usr/sbin
export PATH=/opt/aarch64-toolchain/bin:$PATH
export PATH=/opt/Foundation_v8pkg:$PATH
export PATH=/opt/arm-asianux-linux-gnueabi-toolchain/bin:$PATH

export PS1='[\u@\H \W]\$ '
export HISTSIZE=1000000

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
alias etags=/usr/bin/etags
alias ec='emacsclient -n'
alias mkcscope='find . -name "*.[chxsS]" > cscope.files; cscope -b -q -k'

eo()
{
    if [ $# -ne 1 ]; then
	echo "eo filename" >/dev/stderr
	return 1
    fi
    emacsclient -e "(find-file-other-window \"$1\")"
}
