let g:python_host_prog = '/usr/local/bin/python2.7'
let g:python3_host_prog = '/usr/local/bin/python3'

" Configure vimwiki
let g:vimwiki_list = [{'path': '~/knowledge/', 'syntax': 'markdown', 'ext': '.md'}]
" This config will enable english language in vim.
" Otherwise the language for some reason will be your local
set langmenu=en_US
let $LANG = 'en_US'

"" ----------------------------------------------------------------------------
""
""
"" ===============================
"" | Packages                    |
"" ===============================
""
""
"" ----------------------------------------------------------------------------

" Initialize minpac package manager
packadd minpac
call minpac#init()

" Best and fastest fuzzy finder
set rtp+=/usr/local/opt/fzf
call minpac#add('junegunn/fzf.vim')

" Autocompletion
call minpac#add('Shougo/deoplete.nvim')
let g:deoplete#enable_at_startup = 1

"call minpac#add('tbodt/deoplete-tabnine', { 'do': './install.sh' })

" Nerd Tree for project browsing
call minpac#add('scrooloose/nerdtree')
" Show changed files in nerd tree
call minpac#add('Xuyuanp/nerdtree-git-plugin')

" Mark changed lines of code
call minpac#add('airblade/vim-gitgutter')

" Add git support
call minpac#add('tpope/vim-fugitive')

" CLOJURE GOES HERE

call minpac#add('tpope/vim-fireplace')
call minpac#add('venantius/vim-eastwood')
call minpac#add('venantius/vim-cljfmt')

call minpac#add('morhetz/gruvbox')

" Programming tracker
" call minpac#add('wakatime/vim-wakatime')

" Ruby codesmell analyzer
call minpac#add('rainerborene/vim-reek')

" Dracula color scheme
call minpac#add('dracula/vim')

" fsdf
call minpac#add('dag/vim-fish')

" Vimwiki
call minpac#add('vimwiki/vimwiki')

call minpac#add('rhysd/git-messenger.vim')

call minpac#add('elixir-editors/vim-elixir')

" Linter
call minpac#add('dense-analysis/ale')

" Rails
call minpac#add('tpope/vim-rails')

" Language server (solargraph required)
call minpac#add('autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh'})

" Dash integration
call minpac#add('rizzatti/dash.vim')

call minpac#add('vim-airline/vim-airline')

" Load packages right now
packloadall

"" ----------------------------------------------------------------------------
""
""
"" ===============================
"" | Global configuration        |
"" ===============================
""
""
"" ----------------------------------------------------------------------------

" Disable compatibity layer with VI
set nocompatible

let $NVIM_TUI_ENABLE_TRUE_COLOR = 1

" Turn on mouse support
set mouse=a

" Turn off backup files (SWP)
set nobackup
set nowritebackup
set noswapfile

" Autoread files when change brach on git
set autoread 

" Show line numbers
set number

" Show spaces and tabs
set list
set listchars=tab:▷⋅,trail:⋅,nbsp:⋅

" Visual separator
set colorcolumn=120

" Save undo between sessions
set undofile


" Set tab size
set tabstop=2
set shiftwidth=2
" Use ONLY spaces, not tabs
set expandtab
 " only even number of spaces 
 " (3 spaces + tab = 4 spaces, 2 spaces + tab = 4 spaces)
set smarttab
set nojoinspaces


" Use system clipboard
set clipboard=unnamedplus

" Ignore caseb when searching via vim
set ignorecase

" Setup background
set background=dark

"colorscheme onehalflight
colorscheme dracula

"" ----------------------------------------------------------------------------
""
""
"" ===============================
"" | Key Bindings                |
"" ===============================
""
""
"" ----------------------------------------------------------------------------

" Shorthey for fuzzy finder (not gitignored)
map <leader>t :GFiles<CR>
" Shorthey for fuzzy finder
map <leader>f :FZF<CR>

" move between splits by tab
nnoremap <Tab> <C-w>w

" turn search highlight off
nnoremap <leader><space> :noh<cr>

" open NerdTree file browser
map <leader>o :NERDTreeToggle<cr>

" navigate through autocomplete menu (Deoplete)
inoremap <C-k> <C-Up>
inoremap <C-j> <C-Down>

nnoremap <C-l> :tabnext<CR>
nnoremap <C-h> :tabprevious<CR>
nnoremap <C-n> :tabnew<CR>
tnoremap <Esc> <C-\><C-n>

" Enable linter
let g:ale_linters = {
      \   'ruby': ['standardrb', 'rubocop']
      \}
let g:ale_fix_on_save = 1

function! LinterStatus() abort
  let l:counts = ale#statusline#Count(bufnr(''))

  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors

  return l:counts.total == 0 ? '✨ all good ✨' : printf(
        \   '😞 %dW %dE',
        \   all_non_errors,
        \   all_errors
        \)
endfunction

set statusline=
set statusline+=%m
set statusline+=\ %f
set statusline+=%=
set statusline+=\ %{LinterStatus()}

let g:LanguageClient_serverCommands = {
    \ 'ruby': ['~/.rbenv/shims/solargraph', 'stdio'],
    \ }

" Turn on python3 
let g:python3_host_prog = '/usr/local/bin/python3'


" Move nerdtree to the right
let g:NERDTreeWinPos = "right"
