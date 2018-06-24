function fish_prompt
	if [ $status -eq 0 ]
		set_color -o green
	else
		set_color -o red
	end
	
	echo -n "$USER@"(hostname)"> "
	set_color normal
end

function fish_right_prompt
	if [ $status -eq 0 ]
		set_color -o green
	else
		set_color -o red
	end

	set prompt (echo $PWD | sed "s|^$HOME|~|")
	set len (expr length $prompt)
	set width (tput cols)
	set half_width (expr $width / 2)

	if [ $len -gt $half_width ]
		set start (expr $len - $half_width)
		set prompt (expr substr $prompt $start $len | sed 's|^...|...|')
	end

	echo $prompt
	set_color normal
end

function fish_title; end

alias j "jobs"
alias h "history"
alias ls "ls --color"
alias ll "ls -lh"
alias la "ls -a"
alias e "emacsclient -n"

set fish_greeting

set -x EDITOR vi
set -x PAGER less
