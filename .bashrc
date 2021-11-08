# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

export HISTFILESIZE=""
export HISTSIZE="-1"
export EDITOR="emacsclient -t"
export VISUAL="$EDITOR"

export PATH="/opt/apache-maven-3.3.9/bin:$PATH"
export PATH="~/.local/bin:$PATH"
export PATH="~/bin:$PATH"
export PATH="/opt/java/jdk-11.0.2/bin:$PATH"

export GEM_HOME=$HOME/.gems
export PATH=$HOME/.gems/bin:$PATH

export PS1='\[\e[1;32m\][\u@\h \W]\$\[\e[0m\] '

# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

alias dotfiles='/usr/bin/git --git-dir=/home/wbruschi/.dotfiles/ --work-tree=/home/wbruschi'
