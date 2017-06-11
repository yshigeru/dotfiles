function fish_prompt
	if [ $status -eq 0 ]
		set_color -o cyan
	else
		set_color -o red
	end
	
	echo -n "$USER@"(hostname)" > "
	set_color normal
end

function fish_right_prompt
	set_color -o green

	if [ "$PWD" != "HOME" ]
		echo (echo $PWD | sed "s|^$HOME|~|")
	else
		echo "~"
	end

	set_color normal
end

function fish_title; end

function devshell
	bash -c ". $argv[1] && exec $SHELL"
end

alias j "jobs"
alias h "history"
alias ls "ls --color"
alias ll "ls -l"
alias la "ls -a"
alias ec "emacsclient -n"

set -x EDITOR vi
set -x PAGER less
