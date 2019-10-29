function vim
  nvim $argv
end

set -U EDITOR nvim
set -U LC_ALL en_US.UTF-8
set -x LC_ALL en_US.UTF-8
set -U GOPATH /Users/sqrel/Code/go
set fish_greeting


function cdme
  cd /Users/sqrel/Dropbox/me.txt/
end

function vimconfig
  vim ~/.config/nvim/init.vim
end


function fishconfig
  vim ~/.config/fish/config.fish
end

source (rbenv init - | psub)

set PATH /usr/local/opt/qt@5.5/bin /usr/local/opt/node@8/bin $PATH

set normal (set_color normal)
set magenta (set_color magenta)
set yellow (set_color yellow)
set green (set_color green)
set red (set_color red)
set gray (set_color -o black)

# Fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '+'
set __fish_git_prompt_char_upstream_behind '-'

function fish_prompt
  set last_status $status

  set_color $fish_color_cwd
  printf '%s' (prompt_pwd)
  set_color normal

  printf '%s ' (__fish_git_prompt)

  set_color normal
end

set -g fish_user_paths "/usr/local/opt/elasticsearch@5.6/bin" $fish_user_paths
set -g fish_user_paths "/usr/local/opt/node@8/bin" $fish_user_paths
set -g fish_user_paths "/usr/local/opt/imagemagick@6/bin" $fish_user_paths
