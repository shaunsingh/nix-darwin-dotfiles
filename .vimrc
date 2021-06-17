syntax on 

set ignorecase
set smartcase
set scrolloff=2 " 3 lines above/below cursor when scrolling
let &fcs='eob: '

set clipboard+=unnamed

nmap ; :

inoremap jk <Esc>
cnoremap jk <Esc>

nmap j gj
nmap k gk

nmap <C-h> <c-w>h
nmap <C-l> <c-w>l
nmap <C-k> <c-w>k
nmap <C-j> <c-w>j
