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
Plug 'itchyny/lightline.vim'
Plug 'Yggdroot/indentLine'
Plug 'arcticicestudio/nord-vim'
call plug#end()

let g:lightline = {
      \ 'colorscheme': 'nord',
      \ 'active': {
      \   'left': [
      \     [ 'mode', 'paste' ],
      \     [ 'fugitive', 'filename' ]
      \   ]
      \ },
      \ 'component_function': {
      \   'fugitive': 'LightlineFugitive',
      \   'readonly': 'LightlineReadonly',
      \   'modified': 'LightlineModified',
      \   'filename': 'LightlineFilename'
      \ },
      \ 'separator': {
      \   'left': '',
      \   'right': ''
      \ },
      \ 'subseparator': {
      \   'left': '',
      \   'right': ''
      \ }
    \ }

function! LightlineModified()
  if &filetype == "help"
    return ""
  elseif &modified
    return "+"
  elseif &modifiable
    return ""
  else
    return ""
  endif
endfunction

function! LightlineReadonly()
  if &filetype == "help"
    return ""
  elseif &readonly
    return ""
  else
    return ""
  endif
endfunction

function! LightlineFugitive()
  if exists("*fugitive#head")
    let branch = fugitive#head()
    return branch !=# '' ? ' '.branch : ''
  endif
  return ''
endfunction

function! LightlineFilename()
  return ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
       \ ('' != expand('%:t') ? expand('%:t') : '[No Name]') .
       \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

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
set laststatus=2
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

let g:indentLine_enabled = 0
let g:indentLine_char = '│'

autocmd VimEnter * call SetupLightlineColors()
function SetupLightlineColors() abort
  " transparent background in statusbar
  let l:palette = lightline#palette()

  let l:palette.normal.middle = [ [ 'NONE', 'NONE', 'NONE', 'NONE' ] ]
  let l:palette.inactive.middle = l:palette.normal.middle
  let l:palette.tabline.middle = l:palette.normal.middle

  call lightline#colorscheme()
endfunction
