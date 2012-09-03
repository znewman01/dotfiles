# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
zstyle :compinstall filename '/home/zack/.zshrc'
zstyle ":completion:*:commands" rehash 1

autoload -Uz compinit
compinit

# aliases
setopt completealiases
alias ls='ls --color=auto'
alias halt='sudo /sbin/halt'
alias reboot='sudo /sbin/reboot'


PS1='> '
#PATH=$PATH:$HOME/bin:$HOME/.rvm/bin # Add RVM to PATH for scripting
#EDITOR='vim'

