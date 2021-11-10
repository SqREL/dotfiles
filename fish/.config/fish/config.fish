eval (direnv hook fish)

function vim
  nvim $argv
end

set -U EDITOR nvim
set -U BUNDLER_EDITOR nvim
set -U LC_ALL en_US.UTF-8
set -x LC_ALL en_US.UTF-8
set -x GOPATH $HOME/projects/go
set -U BETTER_ERRORS_EDITOR rubymine
#set -x PGHOST $PGHOST "localhost"
# set fish_greeting
set -u KNOWLEDGEDIR "$HOME/projects/knowledge/"

function ls
  exa $argv
end

function fzfprev
  fzf --preview 'cat {}'
end

function cdme
  cd /Users/sqrel/Dropbox/me.txt/
end

function vimconfig
  vim ~/.config/nvim/init.vim
end


function fishconfig
  vim ~/.config/fish/config.fish
  source ~/.config/fish/config.fish
end

function goto-pi
  ssh pi@192.168.1.88
end

function goto-anvil
  ssh -i ~/.ssh/id_rsa_toptal vasyl.melnychuk@anvil.toptal.net
end

function download-anvil-dump
  scp -i ~/.ssh/id_rsa_toptal vasyl.melnychuk@anvil-app01.staging.toptal.net:/tmp/anvil.sql.gz ~/Downloads/
end

function run_pg
  cd ~/projects/toptal/platform
  docker-compose up postgresql
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

function ct
  cd ~/projects/toptal
end

function gp
  git pull origin $argv
end

function gc
  git checkout $argv
end

# Goto toptal directory
function t
  cd ~/projects/toptal/$argv
end
function to
  cd ~/projects/toptal/$argv
  vim .
end
complete -c t -f -a '(ls -D ~/projects/toptal/)'
complete -c to -f -a '(ls -D ~/projects/toptal/)'

function restart-postgres
  brew services stop postgresql
  rm /usr/local/var/postgres/postmaster.pid
  brew services start postgresql
end

# source (rbenv init - | psub)

set PATH /usr/local/opt/qt@5.5/bin /usr/local/opt/node@8/bin $PATH

set normal (set_color normal)
set magenta (set_color magenta)
set yellow (set_color yellow)
set green (set_color green)
set red (set_color red)
set gray (set_color -o black)
 
 # Fish git prompt
 # set __fish_git_prompt_showdirtystate 'yes'
 # set __fish_git_prompt_showstashstate 'yes'
 # set __fish_git_prompt_showuntrackedfiles 'yes'
 # set __fish_git_prompt_showupstream 'yes'
 # set __fish_git_prompt_color_branch yellow
 # set __fish_git_prompt_color_upstream_ahead green
 # set __fish_git_prompt_color_upstream_behind red
 # 
 # # Status Chars
 # set __fish_git_prompt_char_dirtystate '⚡'
 # set __fish_git_prompt_char_stagedstate '→'
 # set __fish_git_prompt_char_untrackedfiles '☡'
 # set __fish_git_prompt_char_stashstate '↩'
 # set __fish_git_prompt_char_upstream_ahead '+'
 # set __fish_git_prompt_char_upstream_behind '-'
 # 
 # function fish_prompt
 #   set last_status $status
 # 
 #   set_color $fish_color_cwd
 #   printf '%s' (prompt_pwd)
 #   set_color normal
 # 
 #   printf '%s ' (__fish_git_prompt)
 # 
 #   set_color normal
 # end

set -g fish_user_paths "/usr/local/opt/elasticsearch@5.6/bin" $fish_user_paths
#set -g fish_user_paths "/usr/local/opt/node@8/bin" $fish_user_paths
set -g fish_user_paths "/usr/local/opt/imagemagick@6/bin" $fish_user_paths
set -g fish_user_paths "$GOPATH/bin" $fish_user_paths
set -g fish_user_paths "/usr/local/opt/postgresql@11/bin" $fish_user_paths
set -g fish_user_paths "/usr/local/opt/elasticsearch@5.6/bin" $fish_user_paths
set -g fish_user_paths "/usr/local/opt/imagemagick@6/bin" $fish_user_paths

function prepare-for-tests
  docker exec -it toptal_postgresql psql -U toptal -d postgres -c 'DROP DATABASE IF EXISTS toptal_test;'
  git checkout db/schema.rb
  git checkout engines/billing/db/schema.rb
  be rails db:test:prepare
end

function prepare-development
  docker exec -it toptal_postgresql psql -U toptal -d postgres -c 'DROP DATABASE IF EXISTS toptal_development;'
  docker exec -it toptal_postgresql psql -U toptal -d postgres -c 'DROP DATABASE IF EXISTS toptal_test;'
  git checkout db/schema.rb
  git checkout db/analytics/schema.rb
  git checkout engines/billing/db/schema.rb
  bin/setup --verbose
  be rake es:stop
  be rake es:start
  be rails db:test:prepare
end

function prepare-cucumber
  begin
    set -lx RAILS_ENV "cucumber"
    bundle exec rails rails_cache:clear
    bundle exec rake db:drop db:create db:setup
    yarn run build-parallel
  end
end

function vimtoday
  set basedir "$KNOWLEDGEDIR/notes/"
  mkdir -p "$basedir"(date +"%Y")"/"(date +"%m")
  vim "$basedir"(date +"%Y/%m/%d.md")
end

function links
  vim "$KNOWLEDGEDIR/links.md"
end

function know
  cd $KNOWLEDGEDIR
  vim
end

set -g fish_user_paths "/usr/local/opt/node@12/bin" $fish_user_paths
source /Users/vasylmelnychuk/bin/google-cloud-sdk/path.fish.inc
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths
set -g fish_user_paths "/Users/vasylmelnychuk/bin" $fish_user_paths
set -g fish_user_paths "/Users/vasylmelnychuk/.emacs.d/bin" $fish_user_paths

set -U TZ_LIST "America/Recife;Jaimerson Araújo,Africa/Lagos;Samuel Ebeagu,Europe/Warsaw;Juliusz Gonera,Europe/Warsaw;Kamil Lemański,Europe/Kaliningrad;Oleg Polivannyi,Europe/Moscow;Aleksandr Kariakin,Europe/Kiev;Vasyl Melnychuk,"


#starship init fish | source

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/vasylmelnychuk/google-cloud-sdk/path.fish.inc' ]; . '/Users/vasylmelnychuk/google-cloud-sdk/path.fish.inc'; end
set -g fish_user_paths "/usr/local/opt/gnu-getopt/bin" $fish_user_paths

source /usr/local/opt/asdf/asdf.fish
source /usr/local/opt/asdf/libexec/asdf.fish

status --is-interactive; and rbenv init - fish | source
