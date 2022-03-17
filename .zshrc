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