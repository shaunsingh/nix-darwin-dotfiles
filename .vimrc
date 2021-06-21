syntax enable
colorscheme nord

set nobackup
set nocompatible
set noswapfile
set ttyfast
set updatetime=250
set clipboard+=unnamed

set termguicolors
set hidden
set laststatus=0
set lazyredraw
set noerrorbells
"set noshowmode
set shortmess=F
set noruler
set noshowcmd
set novisualbell
"set number
set nowrap
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

nmap ; :

inoremap jk <Esc>
cnoremap jk <Esc>

nmap j gj
nmap k gk

nmap <C-h> <c-w>h
nmap <C-l> <c-w>l
nmap <C-k> <c-w>k
nmap <C-j> <c-w>j

let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"
