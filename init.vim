"autoinstall vim plug
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

"auto install plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

call plug#begin('~/.vim/plugged')

"statusline/bufferline
Plug 'itchyny/lightline.vim'
Plug 'mengelbrecht/lightline-bufferline'

"icons
Plug 'ryanoasis/vim-devicons'
Plug 'bryanmylee/vim-colorscheme-icons'

"fuzzy search + files
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'preservim/nerdtree'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

"minimap"
Plug 'wfxr/minimap.vim', {'do': ':!cargo install --locked code-minimap'}

"smooth scroll
""Plug 'psliwka/vim-smoothie'

"git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

"unix commands
Plug 'tpope/vim-eunuch'

"floating terminal
Plug 'voldikss/vim-floaterm'

"start dash
Plug 'glepnir/dashboard-nvim'

"distraction free/zen mode
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'

"syntax/themes
Plug 'sheerun/vim-polyglot'
Plug 'kaicataldo/material.vim', { 'branch': 'main' }
Plug 'arcticicestudio/nord-vim'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'Brettm12345/moonlight.vim'
Plug 'plasticboy/vim-markdown'

"snake
""Plug 'zyedidia/vim-snake'

"lines on indents
Plug 'Yggdroot/indentLine'

"linting + lsp
Plug 'dense-analysis/ale'
Plug 'maximbaz/lightline-ale'

"animate/adjust
""Plug 'camspiers/animate.vim'
""Plug 'camspiers/lens.vim'

"produce visuals
""Plug 'segeljakt/vim-silicon'

"rich presence
Plug 'andweeb/presence.nvim'
call plug#end()

"set true colors for term + vim
if has('termguicolors')
  set termguicolors
endif
if (has("termguicolors") && $TERM_PROGRAM ==# 'iTerm.app')
  set termguicolors
endif
if (has('nvim'))
  let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
endif
if has("gui_vimr")
  set termguicolors
  set background=dark
  colorscheme nord
endif

"theme info
set background=dark
let g:material_theme_style = 'ocean'
""let g:material_terminal_italics = 1
""colorscheme moonlight
""colorscheme nord
colorscheme material
""colorscheme palenight
syntax on

"set true color
set t_Co=256

"Neovide + gui
set guifont=SFMono\ Nerd\ Font:h13
let g:neovide_cursor_antialiasing=v:true
let g:neovide_fullscreen=v:true

"numrow transparent, vert split line transparent.
highlight clear SignColumn
hi VertSplit ctermfg=1 ctermbg=NONE cterm=NONE
set fillchars+=vert:┊

"highlight current number
""set number
set number relativenumber
set cursorline
highlight clear CursorLine
highlight CursorLineNR ctermbg=red

"auto switch number depending on mode + show numbers in Goyo command mode
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

"various settings
set autoindent                 " Minimal automatic indenting for any filetype.
set backspace=indent,eol,start " Proper backspace behavior.
""set hidden                     " Possibility to have more than one unsaved buffers.
set incsearch                  " Incremental search, hit `<CR>` to stop.
set ruler                      " Shows the current line number at the bottom-right
set wildmenu                   " Better autcomplete
set wildmode=longest,list,full

" Indents word-wrapped lines as much as the 'parent' line
set breakindent
" Ensures word-wrap does not split words
set formatoptions=l
set lbr

"macos clipbard
set clipboard=unnamed
"remove insert from bottom left
set noshowmode
"always display taline and bufferlie"
set laststatus=2
set showtabline=2
"no swap files, no backus
set noswapfile
set nobackup

"lightline setup
"colorscheme, bottom bar, coponents, tabs, bottom components
let g:lightline = {
    \ 'colorscheme': 'material',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'gitbranch', 'readonly', 'filename', 'wordcount', 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ] ]
    \ },
    \ 'component_function': {
    \    'filetype': 'MyFiletype',
    \    'fileformat': 'MyFileformat',
    \    'wordcount': 'WordCount',
    \    'gitbranch': 'FugitiveHead'
    \ },
    \ 'tabline': {
    \   'left': [ ['buffers'] ],
    \   'right': [ ['close'] ]
    \ },
    \ 'component_expand': {
    \   'buffers': 'lightline#bufferline#buffers',
    \   'linter_checking': 'lightline#ale#checking',
    \   'linter_infos': 'lightline#ale#infos',
    \   'linter_warnings': 'lightline#ale#warnings',
    \   'linter_errors': 'lightline#ale#errors',
    \   'linter_ok': 'lightline#ale#ok',
    \ },
    \ 'component_type': {
    \   'buffers': 'tabsel',
    \   'linter_checking': 'right',
    \   'linter_infos': 'right',
    \   'linter_warnings': 'warning',
    \   'linter_errors': 'error',
    \   'linter_ok': 'right',
    \ },
    \ 'component': {
    \   'lineinfo': '%3l:%-2v%<',
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

"Make fzf float
let $FZF_DEFAULT_COMMAND =  "find * -path '*/\.*' -prune -o -path 'node_modules/**' -prune -o -path 'target/**' -prune -o -path 'dist/**' -prune -o  -type f -print -o -type l -print 2> /dev/null"
let $FZF_DEFAULT_OPTS=' --color=dark --color=fg:15,bg:-1,hl:1,fg+:#ffffff,bg+:0,hl+:1 --color=info:0,prompt:0,pointer:12,marker:4,spinner:11,header:-1 --layout=reverse  --margin=1,4'
let g:fzf_layout = { 'window': 'call FloatingFZF()' }

"hardcoded atm sorry
function! FloatingFZF()
  let buf = nvim_create_buf(v:false, v:true)
  call setbufvar(buf, '&signcolumn', 'no')

  let height = float2nr(30)
  let width = float2nr(150)
  let horizontal = float2nr((&columns - width) / 2)
  ""let vertical = 1
  let vertical = 12

  let opts = {
        \ 'relative': 'editor',
        \ 'row': vertical,
        \ 'col': horizontal,
        \ 'width': width,
        \ 'height': height,
        \ 'style': 'minimal'
        \ }

  call nvim_open_win(buf, v:true, opts)
endfunction        return b:wordcount
    endif
endfunction

"always mouse available
if has('mouse')
  set mouse=a
endif

"fzf
nnoremap <silent> <C-p> :call fzf#vim#files('.', {'options': '--prompt ""'})<CR>
"autoclose
inoremap " ""<left>
""inoremap ' ''<left>
inoremap ( ()<left>
inoremap [ []<left>
inoremap { {}<left>
inoremap {<CR> {<CR>}<ESC>O
inoremap {;<CR> {<CR>};<ESC>O

"jk to exit instead
" esc in insert mode
inoremap jk <esc>

" esc in command mode
cnoremap jk <C-C>
" Note: In command mode mappings to esc run the command for some odd
" historical vi compatibility reason. We use the alternate method of
" existing which is Ctrl-C

"basic vim wm (ctrl + hjkl to move/create)
function! WinMove(key)
    let t:curwin = winnr()
    exec "wincmd ".a:key
    if (t:curwin == winnr())
        if (match(a:key,'[jk]'))
            wincmd v
        else
            wincmd s
        endif
        exec "wincmd ".a:key
    endif
endfunction
nnoremap <silent> <C-h> :call WinMove('h')<CR>
nnoremap <silent> <C-j> :call WinMove('j')<CR>
nnoremap <silent> <C-k> :call WinMove('k')<CR>
nnoremap <silent> <C-l> :call WinMove('l')<CR>

"f5 to run current filetype
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
exec "!time python3 %"
elseif &filetype == 'html'
exec "!firefox % &"
elseif &filetype == 'go'
exec "!go build %<"
exec "!time go run %"
endif
endfunc

"lightline ale
let g:lightline#ale#indicator_checking = "\uf110"
let g:lightline#ale#indicator_infos = "\uf129"
let g:lightline#ale#indicator_warnings = "\uf071"
let g:lightline#ale#indicator_errors = "\uf05e"
let g:lightline#ale#indicator_ok = "\uf00c"

"just ale icons
let g:ale_sign_error                  = '✘'
let g:ale_sign_warning                = '⚠'
highlight ALEErrorSign ctermbg        =NONE ctermfg=red
highlight ALEWarningSign ctermbg      =NONE ctermfg=yellow

let g:ale_linters = {
            \   'mail': ['proselint'],
            \   'markdown': ['proselint'],
            \   'text': ['proselint'],
	    \   'python': ['pyls'],
            \   }
let g:ale_fixers = {
\   '*':          ['remove_trailing_lines', 'trim_whitespace'],
\}

let g:ale_lint_on_text_changed = 'never'
let g:ale_fix_on_save = 1
let g:ale_lint_on_save = 1

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

"indentline
let g:indentLine_enabled = 0
let g:indentLine_char_list = ['┊']

"NERDTree
let g:webdevicons_enable = 1
let g:webdevicons_enable_nerdtree = 1
let g:webdevicons_conceal_nerdtree_brackets = 1
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1
let g:NERDTreeLimitedSyntax = 1
let g:NERDTreeHighlightCursorline = 0

"dashboard
let g:dashboard_default_executive ='fzf'
let g:dashboard_custom_header =<< trim END
=================     ===============     ===============   ========  ========
\\ . . . . . . .\\   //. . . . . . .\\   //. . . . . . .\\  \\. . .\\// . . //
||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\/ . . .||
|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||
||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||
|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\ . . . . ||
||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\_ . .|. .||
|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\ `-_/| . ||
||_-' ||  .|/    || ||    \|.  || `-_|| ||_-' ||  .|/    || ||   | \  / |-_.||
||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \  / |  `||
||    `'         || ||         `'    || ||    `'         || ||   | \  / |   ||
||            .===' `===.         .==='.`===.         .===' /==. |  \/  |   ||
||         .=='   \_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \/  |   ||
||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \/  |   ||
||   .=='    _-'          '-__\._-'         '-_./__-'         `' |. /|  |   ||
||.=='    _-'                                                     `' |  /==.||
=='    _-'                        N E O V I M                         \/   `==
\   _-'                                                                `-_   /
 `''
END

"minimap
let g:minimap_width = 10
let g:minimap_auto_start = 1
let g:minimap_auto_start_win_enter = 1
let g:lens#disabled_filetypes = ['minimap', 'Minimap', '-MINIMAP-', 'fzf', 'nvim-tree']

"limelight
let g:limelight_default_coefficient = 0.4
""let g:limelight_conceal_ctermfg = 141
let g:limelight_paragraph_span = 0
""let g:limelight_conceal_guifg = '#ECEFF4'

"drp
let g:presence_auto_update       = 1
let g:presence_editing_text      = "Editing %s"
let g:presence_workspace_text    = "Working on %s"
let g:presence_neovim_image_text = "The One True Text Editor"
let g:presence_main_image        = "neovim"
let g:presence_client_id         = "793271441293967371"
let g:presence_debounce_timeout  = 15

"Goyo
let g:goyo_width = 85
""let g:goyo_height = 80

"limelight +goyo
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

