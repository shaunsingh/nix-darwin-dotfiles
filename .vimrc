call plug#begin('~/.vim/plugged')
Plug 'Brettm12345/moonlight.vim'
"Plug 'ayu-theme/ayu-vim'
"Plug 'drewtempelmeyer/palenight.vim'
Plug 'itchyny/lightline.vim'
Plug 'preservim/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'thaerkh/vim-workspace'
Plug 'psliwka/vim-smoothie'
Plug 'mg979/vim-visual-multi', {'branch': 'master'}
Plug 'sheerun/vim-polyglot'
Plug 'ryanoasis/vim-devicons'
Plug 'tpope/vim-eunuch'
Plug 'TaDaa/vimade'
Plug 'severin-lemaignan/vim-minimap'
call plug#end()

"set theme
set background=dark
if has('termguicolors')
set termguicolors
endif
let g:moonlight_termcolors=256
colorscheme moonlight
syntax on
"let ayucolor="mirage"

"set true color
set t_Co=256
highlight Normal ctermbg=NONE
highlight nonText ctermbg=NONE

"tell number row to be transparent
highlight clear LineNr
highlight clear SignColumn

"general ui
set number
set laststatus=2
set encoding=UTF-8
set ttyfast
set lazyredraw

" vim-devicons
let g:lightline = {
    \ 'component_function': {
    \    'filetype': 'MyFiletype',
    \    'fileformat': 'MyFileformat',
    \ }
    \ }

"add devicon to lightline
function! MyFiletype()
    return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
endfunction

function! MyFileformat()
    return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
endfunction

"always mouse available
if has('mouse')
  set mouse=a
endif

"wrap markdown files
augroup Markdown
  autocmd!
  autocmd FileType markdown set wrap
augroup END

"binds
map <C-i> gt
nnoremap <c-p> :Files /Users/shauryasingh/IdeaProjects<CR>
map <C-m> :Minimap<CR>
" Map Control S for save
noremap <silent> <C-S> :update<CR>
vnoremap <silent> <C-S> <C-C>:update<CR>
inoremap <silent> <C-S>  <C-O>:update<CR>
noremap <silent> <C-f> ::NERDTreeToggle<CR>
map <C-a> <esc>ggVG<CR>

inoremap " ""<left>
inoremap ' ''<left>
inoremap ( ()<left>
inoremap [ []<left>
inoremap { {}<left>
inoremap {<CR> {<CR>}<ESC>O
inoremap {;<CR> {<CR>};<ESC>O

"fzf colors
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

" vim-devicons for folders
let g:DevIconsEnableFoldersOpenClose = 1
let g:webdevicons_conceal_nerdtree_brackets = 0
let g:WebDevIconsOS = 'Darwin'
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols = {}

"NERDTree info
let g:NERDTreeHighlightFolders = 1
let g:NERDTreeHighlightFoldersFullName = 1

"theme info
let g:moonlight_terminal_italics=1
let g:lightline.colorscheme = 'moonlight'

let s:palette = g:lightline#colorscheme#{g:lightline.colorscheme}#palette
let s:palette.normal.middle = [ [ 'NONE', 'NONE', 'NONE', 'NONE' ] ]
let s:palette.inactive.middle = s:palette.normal.middle
let s:palette.tabline.middle = s:palette.normal.middle

autocmd VimEnter * NERDTree
au VimEnter * wincmd l
