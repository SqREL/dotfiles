let g:python3_host_prog = '/usr/local/bin/python3'

"dein Scripts-----------------------------

if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath^=/Users/sqrel/.config/nvim/dein/repos/github.com/Shougo/dein.vim

" Required:
call dein#begin(expand('/Users/sqrel/.config/nvim/dein'))

" Let dein manage dein
" Required:
call dein#add('Shougo/dein.vim')

" Snippets
call dein#add('Shougo/neosnippet.vim')
call dein#add('Shougo/neosnippet-snippets')
call dein#add('Shougo/neco-vim')
call dein#add('Shougo/neoinclude.vim')
" call dein#add('Shougo/neco-synax')

" Colorscheme
call dein#add('mhartington/oceanic-next')

" Autocoplete
call dein#add('Shougo/deoplete.nvim')

" File managers
call dein#add('scrooloose/nerdtree')
call dein#add('Xuyuanp/nerdtree-git-plugin')

" Fuzzy finder
call dein#add('junegunn/fzf')

" Syntax checker
call dein#add('scrooloose/syntastic')

" Rubocop
call dein#add('ngmy/vim-rubocop')

" Print vertical lines at each indentation level
call dein#add('Yggdroot/indentLine')

" Auto closing of quotes
call dein#add('Raimondi/delimitMate')

" Elixir support
call dein#add('elixir-lang/vim-elixir')

" Coffee support
call dein#add('kchmck/vim-coffee-script')

" Git
call dein#add('tpope/vim-fugitive')
call dein#add('airblade/vim-gitgutter')

call dein#add('slim-template/vim-slim')

" Statusline
" call dein#add('vim-airline/vim-airline')

" Quotes manipulations
call dein#add('tpope/vim-surround')

" Comments helper
call dein#add('tomtom/tcomment_vim')

" Ruby beautifier
call dein#add('Chiel92/vim-autoformat')

" Autocomplete for ruby blocks
call dein#add('tpope/vim-endwise')

" Autocomplete for ruby blocks
call dein#add('tpope/vim-rails')

call dein#end()

"end dein Scripts-----------------------------

filetype plugin indent on


"------------------Config-------------------
let $NVIM_TUI_ENABLE_TRUE_COLOR = 1

syntax enable
colorscheme OceanicNext
set background=dark
" let g:airline_theme='oceanicnext'
" let g:airline_powerline_fonts=1

" Turn on python3 
let g:python3_host_prog = '/usr/local/bin/python3'

" Turn on mouse support
set mouse=a

" Turn off backup files
set nobackup
set nowritebackup
set noswapfile

" Autoread files when change brach on git
set autoread 

" Show filename in title
set title

" Minimum indentation from top/bottom of screen to highlighted result on
" search
set scrolloff=3

" Show line numbers
set number

" Undo history
set history=1000

" Disable wrapping string
set nowrap

" Show spaces and tabs
set list
set listchars=tab:▷⋅,trail:⋅,nbsp:⋅

" New split must be created
set splitbelow splitright

" Allow hidden buffers
set hidden

" Github width
" set colorcolumn=110

" Save undo between sessions
set undofile

" Set tab size
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab " replace tabs to spaces
set smarttab " only even number of spaces (3 spaces + tab = 4 spaces, 2 spaces + tab = 4 spaces)

" Use system clipboard
set clipboard=unnamedplus

" Enable autocoplete
let g:deoplete#enable_at_startup = 1

" Disable async in gitgutter
let g:gitgutter_async = 0

" --------------------Mappings---------------------

" Search with regexp by default
nnoremap / /\v
vnoremap / /\v
set ignorecase

" Smart way to move between windows
" map <C-j> <C-W>j
" map <C-k> <C-W>k
" map <C-h> <C-W>h
" map <C-l> <C-W>l
" move between splits by tab
nnoremap <Tab> <C-w>w
" turn search highlight off
nnoremap <leader><space> :noh<cr>

" open file browser
map <leader>o :NERDTreeToggle<cr>
" set cursor in file browser on current file
map <C-f> :NERDTreeFind<cr>

" Easy commenting
nnoremap // :TComment<CR>
vnoremap // :TComment<CR>

" navigate through autocomplete menu (Deoplete)
inoremap <C-k> <C-Up>
inoremap <C-j> <C-Down>

" Shorthey for fuzzy finder
map <leader>t :FZF<CR>

" Set cusom config file for RuboCop
let g:vimrubocop_config = '~/.config/nvim/rubocop.yml'


set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
