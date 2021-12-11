# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Environment variables

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

export HISTFILESIZE=1000000
export HISTSIZE=1000000
export EDITOR=nano
export VISUAL="$EDITOR"

# Tell ls to be colourful
export CLICOLOR=1
export LSCOLORS=Exfxcxdxbxegedabagacad

# Tell grep to highlight matches
export GREP_OPTIONS='--color=auto'

export TERM="xterm"
export PS1='\[\e[32m\]\u@\h \W\[\e[0m\] $ '

# Ruby
export GEM_HOME=$HOME/.gems

# PATH
export PATH="~/.local/bin:$PATH"
export PATH="~/bin:$PATH"
export PATH="/Applications/Postgres.app/Contents/Versions/latest/bin:$PATH"
export PATH=$HOME/.gems/bin:$PATH

# User specific aliases and functions

alias ll="ls -l"
alias dotfiles="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"

# Machine specific settings
if [ -f ~/.bashrc_local ]; then
	. ~/.bashrc_local
fi
