if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

export HISTFILESIZE=1000000
export HISTSIZE=1000000
export EDITOR=nano
export VISUAL="$EDITOR"

# Tell ls to be colourful
export CLICOLOR=1
export LSCOLORS=Exfxcxdxbxegedabagacad

# Tell grep to highlight matches
export GREP_OPTIONS='--color=auto'

export TERM="xterm-color"
export PS1='\[\e[32m\]\u@\h \W\[\e[0m\] $ '

# Ruby
export GEM_HOME=$HOME/.gems

# PATH
export PATH="~/.local/bin:$PATH"
export PATH="~/bin:$PATH"
export PATH="/Applications/Postgres.app/Contents/Versions/latest/bin:$PATH"
export PATH=$HOME/.gems/bin:$PATH
