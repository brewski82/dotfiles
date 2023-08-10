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

# https://github.com/akermu/emacs-libvterm#shell-side-configuration
if [ -f ~/.emacs-vterm-bash.sh ]; then
	. ~/.emacs-vterm-bash.sh
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
