# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="greenlantern"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias update="sudo pacman -Syu"
alias install="sudo pacman -S"
alias vi="vim"
alias v="vim"
alias c="cd ~/Dropbox/Code/"
alias pc="ssh sqrel@192.168.0.100"
alias wake_pc="wol 00:25:22:4c:04:3a"
alias wc="cd ~/Work/Code"

alias mount_store='sudo mount -t 192.168.0.100:/srv/nfs4/Store /home/sqrel/Network/Store'
alias mount_home='sudo mount -t 192.168.0.100:/srv/nfs4/sqrel /home/sqrel/Network/pc_home'
alias umount_store='sudo umount /home/sqrel/Network/Store'
alias umount_home='sudo umount /home/sqrel/Network/pc_home'

# TODO: Make this for rbenv
#alias uj='rvm use jruby@heap'
#alias ur='rvm use ruby@heap'
#alias ux='rvm use rbx@heap'

alias dhome='HOME=$HOME/.dropbox-personal/ setsid /usr/bin/dropbox start -i'
alias dwork='HOME=$HOME/.dropbox-work/ setsid /usr/bin/dropbox start -i'

alias rake="noglob rake"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git rbenv rails ruby autojump bundler compleat gem git-flow gradle rake zeus)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

PATH="$PATH:/usr/local/heroku/bin:$HOME/.dotfiles/bin"

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
# Ruby speed-up
export RUBY_HEAP_MIN_SLOTS=1000000
export RUBY_HEAP_FREE_MIN=500000
export RUBY_HEAP_SLOTS_INCREMENT=1000000
export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
export RUBY_GC_MALLOC_LIMIT=100000000
