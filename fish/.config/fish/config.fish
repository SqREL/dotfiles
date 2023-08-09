eval (/opt/homebrew/bin/brew shellenv)
eval (direnv hook fish)

function vim
  nvim $argv
end

set -U EDITOR nvim
set -U BUNDLER_EDITOR nvim
set -U LC_ALL en_US.UTF-8
set -x LC_ALL en_US.UTF-8
set -U BETTER_ERRORS_EDITOR nvim

function ls
  exa $argv
end

function fzfprev
  fzf --preview 'cat {}'
end

function vimconfig
  vim ~/.config/nvim/init.vim
end

function fishconfig
  vim ~/.config/fish/config.fish
  source ~/.config/fish/config.fish
end

function goto-mac
  ssh sqrel@100.117.19.56
end

function goto-linux
  ssh vmelnychuk@100.96.90.52
end

# Bumdler 
function be
  bundle exec $argv
end

function ber
  bundle exec rubocop $argv
end

function bi
  bundle install
end

function gp
  git pull origin $argv
end

function gc
  git checkout $argv
end

# Goto sphera directory
function cs
  cd ~/projects/sphera
end

function s
  cd ~/projects/sphera/$argv
end
complete -c s -f -a '(ls -D ~/projects/sphera/)'

function restart-postgres
  brew services stop postgresql
  rm /usr/local/var/postgres/postmaster.pid
  brew services start postgresql
end

set normal (set_color normal)
set magenta (set_color magenta)
set yellow (set_color yellow)
set green (set_color green)
set red (set_color red)
set gray (set_color -o black)
 
set -g fish_user_paths "$HOME/.emacs.d/bin" $fish_user_paths

set PATH /opt/homebrew/opt/python@3.10/libexec/bin $PATH

status --is-interactive; and rbenv init - fish | source

source /opt/homebrew/opt/asdf/libexec/asdf.fish
