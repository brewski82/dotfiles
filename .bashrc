# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Source my settings
if [ -f ~/.myshell ]; then
	. ~/.myshell
fi

# Prompt
eval "$(starship init bash)"

# Machine specific settings
if [ -f ~/.bashrc_local ]; then
	. ~/.bashrc_local
fi

# git autocomplete
if [ -f `brew --prefix`/etc/bash_completion.d/git-completion.bash ]; then
  . `brew --prefix`/etc/bash_completion.d/git-completion.bash
fi
