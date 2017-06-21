call plug#begin('~/.local/share/nvim/plugged')

Plug 'altercation/Vim-colors-solarized'
Plug 'racer-rust/vim-racer'
Plug 'valloric/YouCompleteMe'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'rust-lang/rust.vim'
Plug 'idris-hackers/idris-vim'
Plug 'vim-syntastic/syntastic'
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'sjl/gundo.vim'
Plug 'LnL7/vim-nix'
call plug#end()

set termguicolors
colorscheme solarized
set background=dark

let g:ycm_rust_src_path = '/usr/local/src/rust/src'
let g:rustfmt_autosave = 1
let g:airline#extensions#tabline#enabled = 1
syntax enable
set number " Line numbers
set hidden
set textwidth=80
set smartindent
set noexpandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set cursorline
set clipboard=unnamed
set clipboard=unnamedplus

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0


" move vertically by visual line
nnoremap j gj
nnoremap k gk

nnoremap <S-j> :prev<CR>
nnoremap <S-k> :n<CR>

filetype plugin indent on

let mapleader=" "
nnoremap <leader>u :GundoToggle<CR>

map <F4> :RustRun<CR>

let g:racer_cmd = "$HOME/.cargo/bin/racer/target/release/racer"
let $RUST_SRC_PATH="/usr/local/src/rust/src/"

imap jj <esc>

set wrap
set linebreak
set nolist
set wrapmargin=0
set textwidth=0
set wildmenu
set lazyredraw
set showmatch
set incsearch
set hlsearch
nnoremap <leader><space> :nohlsearch<CR>
set foldenable
set foldlevelstart=10
set foldnestmax=10

