" Install vim-plug if not found
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

call plug#begin(expand('~/.vim/plugged'))
Plug 'arcticicestudio/nord-vim'
call plug#end()

syntax enable
colorscheme nord

set nobackup
set nocompatible
set noswapfile
set ttyfast
set viminfo=
set updatetime=250
set clipboard+=unnamed

set termguicolors
set hidden
set laststatus=0
set lazyredraw
set noerrorbells
"set noshowmode
set novisualbell
"set number
set nowrap
set ruler
set t_vb=
set tm=500
set mouse=a
let &fcs='eob: '

set ignorecase
set smartcase
set hlsearch
set incsearch

set scrolloff=2
set autoindent
set backspace=indent,eol,start
set colorcolumn=160
set expandtab
set foldcolumn=1
set foldenable

nmap ; :

inoremap jk <Esc>
cnoremap jk <Esc>

nmap j gj
nmap k gk

nmap <C-h> <c-w>h
nmap <C-l> <c-w>l
nmap <C-k> <c-w>k
nmap <C-j> <c-w>j
