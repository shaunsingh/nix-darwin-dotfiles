call plug#begin('~/.vim/plugged')
Plug 'Brettm12345/moonlight.vim'
Plug 'hzchirs/vim-material'
Plug 'itchyny/lightline.vim'
Plug 'preservim/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'ervandew/supertab'
Plug 'thaerkh/vim-workspace'
Plug 'mkitt/tabline.vim'
Plug 'wincent/command-t'
Plug 'psliwka/vim-smoothie'
Plug 'mg979/vim-visual-multi', {'branch': 'master'}
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-surround'
Plug 'ashisha/image.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'tpope/vim-eunuch'
Plug 'dense-analysis/ale'
Plug 'https://github.com/drzel/vim-repo-edit.git'
Plug 'TaDaa/vimade'
Plug 'suan/vim-instant-markdown', {'for': 'markdown'}
Plug 'severin-lemaignan/vim-minimap' 
call plug#end()

set background=dark

if has('termguicolors')
set termguicolors
endif
let g:moonlight_termcolors=16

colorscheme moonlight
syntax on 

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

function! MyFiletype()
    return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
endfunction

function! MyFileformat()
    return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
endfunction

if has('mouse')
  set mouse=a
endif

augroup Markdown
  autocmd!
  autocmd FileType markdown set wrap
augroup END

map <C-i> gt
nnoremap <c-p> :Files /Users/shauryasingh/IdeaProjects<CR>
map <C-m> :Minimap<CR>
" Map Control S for save
noremap <silent> <C-S> :update<CR>
vnoremap <silent> <C-S> <C-C>:update<CR>
inoremap <silent> <C-S>  <C-O>:update<CR>
noremap <silent> <C-f> ::NERDTreeToggle<CR>
map <C-a> <esc>ggVG<CR>

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

let g:ale_sign_error                  = '✘'
let g:ale_sign_warning                = '⚠'
highlight ALEErrorSign ctermbg        =NONE ctermfg=red
highlight ALEWarningSign ctermbg      =NONE ctermfg=yellow
let g:ale_linters_explicit            = 1
let g:ale_lint_on_text_changed        = 'never'
let g:ale_lint_on_enter               = 0
let g:ale_lint_on_save                = 1
let g:ale_fix_on_save                 = 1

let g:ale_linters = {
\   'markdown':      ['mdl', 'writegood'],
\}

let g:ale_fixers = {
\   '*':          ['remove_trailing_lines', 'trim_whitespace'],
\}

" vim-devicons
let g:DevIconsEnableFoldersOpenClose = 1

let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols = {}

let g:NERDTreeHighlightFolders = 1
let g:NERDTreeHighlightFoldersFullName = 1

let g:moonlight_terminal_italics=1
let g:lightline.colorscheme = 'moonlight'

autocmd VimEnter * NERDTree


