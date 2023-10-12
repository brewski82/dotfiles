# .zshrc

# Source global definitions
if [ -f /etc/zshrc ]; then
	. /etc/zshrc
fi

# Source my settings
if [ -f ~/.myshell ]; then
	. ~/.myshell
fi

export HISTFILE=~/.zsh_history
setopt appendhistory

# Prompt
eval "$(starship init zsh)"

# Machine specific settings
if [ -f ~/.zshrc_local ]; then
	. ~/.zshrc_local
fi

# https://github.com/akermu/emacs-libvterm#shell-side-configuration
if [ -f ~/.emacs-vterm-zsh.sh ]; then
	. ~/.emacs-vterm-zsh.sh
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
