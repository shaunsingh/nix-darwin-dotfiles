call plug#begin('~/.vim/plugged')
Plug 'Brettm12345/moonlight.vim'
Plug 'itchyny/lightline.vim'
Plug 'preservim/nerdtree'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'psliwka/vim-smoothie'
Plug 'mg979/vim-visual-multi', {'branch': 'master'}
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-fugitive'
Plug 'ryanoasis/vim-devicons'
Plug 'TaDaa/vimade'
Plug 'severin-lemaignan/vim-minimap'
Plug 'tpope/vim-eunuch'
Plug 'mhinz/vim-startify'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'scrooloose/syntastic'
Plug 'lifepillar/vim-mucomplete'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
call plug#end()


"set theme
set background=dark

if has('termguicolors')
set termguicolors
endif

if (has("termguicolors") && $TERM_PROGRAM ==# 'iTerm.app')
  set termguicolors
endif

let g:moonlight_termcolors=256
colorscheme moonlight
syntax on

"set true color
set t_Co=256
highlight Normal ctermbg=NONE
highlight nonText ctermbg=NONE


"tell number row to be transparent
highlight clear LineNr
highlight clear SignColumn

"various settings
set autoindent                 " Minimal automatic indenting for any filetype.
set backspace=indent,eol,start " Proper backspace behavior.
set hidden                     " Possibility to have more than one unsaved buffers.
set incsearch                  " Incremental search, hit `<CR>` to stop.
set ruler                      " Shows the current line number at the bottom-right
                               " of the screen.
set wildmenu                   " Great command-line completion, use `<Tab>` to move
                               " around and `<CR>` to validate.
set number

"lightline setup
let g:lightline = {
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'gitbranch', 'readonly', 'filename', 'modified', 'wordcount' ] ]
    \ },
    \ 'component_function': {
    \    'filetype': 'MyFiletype',
    \    'fileformat': 'MyFileformat',
    \    'wordcount': 'WordCount',  
    \    'gitbranch': 'FugitiveHead'
    \ }
    \ }

"add devicon to lightline
function! MyFiletype()
    return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
endfunction

function! MyFileformat()
    return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
endfunction

"create function to get workdcount
function! WordCount()
    let currentmode = mode()
    if !exists("g:lastmode_wc")
        let g:lastmode_wc = currentmode
    endif
    " if we modify file, open a new buffer, be in visual ever, or switch modes
    " since last run, we recompute.
    if &modified || !exists("b:wordcount") || currentmode =~? '\c.*v' || currentmode != g:lastmode_wc
        let g:lastmode_wc = currentmode
        let l:old_position = getpos('.')
        let l:old_status = v:statusmsg
        execute "silent normal g\<c-g>"
        if v:statusmsg == "--No lines in buffer--"
            let b:wordcount = 0
        else
            let s:split_wc = split(v:statusmsg)
            if index(s:split_wc, "Selected") < 0
                let b:wordcount = str2nr(s:split_wc[11])
            else
                let b:wordcount = str2nr(s:split_wc[5])
            endif
            let v:statusmsg = l:old_status
        endif
        call setpos('.', l:old_position)
        return b:wordcount
    else
        return b:wordcount
    endif
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


"binds (indent, fzf, undo, undo)
map <C-i> gt
nnoremap <c-p> :Files /Users/shauryasingh/IdeaProjects<CR>
nnoremap <C-Z> u
inoremap <C-Z> <C-O>u

"autoclose
inoremap " ""<left>
inoremap ' ''<left>
inoremap ( ()<left>
inoremap [ []<left>
inoremap { {}<left>
inoremap {<CR> {<CR>}<ESC>O
inoremap {;<CR> {<CR>};<ESC>O

"f5 to run current fil
map <F5> :call CompileRunGcc()<CR>
func! CompileRunGcc()
exec "w"
if &filetype == 'c'
exec "!gcc % -o %<"
exec "!time ./%<"
elseif &filetype == 'cpp'
exec "!g++ % -o %<"
exec "!time ./%<"
elseif &filetype == 'java'
exec "!javac %"
exec "!time java -cp %:p:h %:t:r"
elseif &filetype == 'sh'
exec "!time bash %"
elseif &filetype == 'python'
exec "!time python2.7 %"
elseif &filetype == 'html'
exec "!firefox % &"
elseif &filetype == 'go'
exec "!go build %<"
exec "!time go run %"
elseif &filetype == 'mkd'
exec "!~/.vim/markdown.pl % > %.html &"
exec "!firefox %.html &"
endif
endfunc

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

"Autocomplete info
set completeopt+=menuone
  set shortmess+=c   " Shut off completion messages
  set belloff+=ctrlg " If Vim beeps during completion
  let g:mucomplete#enable_auto_at_startup = 1
  let g:mucomplete#completion_delay = 1

setlocal dictionary+=spell
setlocal complete+=k

let g:mucomplete#chains = {
                  \ 'default':    ['file', 'keyn', 'omni', 'user', 'defs', 'incl', 'c-n', 'uspl'],
                  \ 'vim':        ['file', 'keyn', 'cmd',  'omni', 'user', 'c-n', 'uspl'],
                  \ 'text':       ['file', 'c-n',  'uspl', 'omni', 'user'],
                  \ }

let g:mucomplete#can_complete = {}
let g:mucomplete#can_complete.default = {
        \     'c-n' : { t -> t =~# '\v\k{2,}$' },
        \     'uspl': { t -> t =~# '\v\k{3,}$' && &l:spell && !empty(&l:spelllang) },
        \ }

"vimade config
let g:vimade = {}
let g:vimade.fadelevel = 0.7
let g:vimade.enablesigns = 1

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
let g:lightline.colorscheme = 'material'

"syntastic info
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

"limelight for goyo 
let g:limelight_default_coefficient = 0.4
let g:limelight_conceal_ctermfg = 141
let g:limelight_paragraph_span = 0

"make cursor line -> block
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_SR = "\<Esc>]50;CursorShape=2\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"

"anti-delay for above
set ttimeout
set ttimeoutlen=1
set listchars=tab:>-,trail:~,extends:>,precedes:<,space:.
set ttyfast

"make statusline transparent (kindof working)
autocmd VimEnter * call SetupLightlineColors()
function SetupLightlineColors() abort
  " transparent background in statusbar
  let l:palette = lightline#palette()

  let l:palette.normal.middle = [ [ 'NONE', 'NONE', 'NONE', 'NONE' ] ]
  let l:palette.insert.middle = l:palette.normal.middle
  let l:palette.visual.middle = l:palette.normal.middle
  let l:palette.inactive.middle = l:palette.normal.middle
  let l:palette.inactive.middle = l:palette.normal.middle
  let l:palette.tabline.middle = l:palette.normal.middle

  call lightline#colorscheme()
endfunction

"open intro, then nerdtree and minimap
autocmd VimEnter * intro
autocmd VimEnter * NERDTree
autocmd VimEnter * Minimap

"auto center
au VimEnter * wincmd l

"limelight +goyo
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

"if using .md then open goyo
autocmd BufRead,BufNewFile *.md :Goyo 80



