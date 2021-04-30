set nocompatible


"__PLUGINS__"


"autoinstall vim plug
"check for uninstalled plugins
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
Plug 'ojroques/vim-scrollstatus'

"icons
Plug 'ryanoasis/vim-devicons'

"fuzzy search + files
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'tiagofumo/vim-nerdtree-syntax-highlight', { 'on': 'NERDTreeToggle' }

"minimap // enable when supported in openGL neovide
""Plug 'wfxr/minimap.vim', {'do': ':!cargo install --locked code-minimap'}

"git
Plug 'tpope/vim-fugitive', { 'on': [] }
""Plug 'airblade/vim-gitgutter'

"unix commands
Plug 'tpope/vim-eunuch'

"start dash + give a tip
Plug 'glepnir/dashboard-nvim'

"distraction free/zen mode
Plug 'kdav5758/TrueZen.nvim'
Plug 'junegunn/goyo.vim', {'on': 'Goyo' }
Plug 'junegunn/limelight.vim', {'on': 'Goyo' }

"syntax/themes (treesitter replacing polygot)
""Plug 'arcticicestudio/nord-vim'
""Plug 'Brettm12345/moonlight.vim'
""Plug 'GustavoPrietoP/doom-one.vim'
Plug 'shaunsingh/material.nvim'
""Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

"add color to hex
Plug 'norcalli/nvim-colorizer.lua'

"markdown writing
Plug 'kana/vim-textobj-user', {'for': 'markdown'}
Plug 'preservim/vim-textobj-quote', {'for': 'markdown'}
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
"kotlin support
Plug 'udalov/kotlin-vim', {'for': 'kotlin'}

"writing plugins
Plug 'preservim/vim-litecorrect'

"lines on indents + auto pairs+ multiple cursors
Plug 'Yggdroot/indentLine'
Plug 'lukas-reineke/indent-blankline.nvim'
Plug 'mg979/vim-visual-multi', {'branch': 'master'}

"linting + lsp
Plug 'dense-analysis/ale'
Plug 'maximbaz/lightline-ale'

"rich presence
""Plug 'andweeb/presence.nvim'

"easymotions
Plug 'phaazon/hop.nvim'

"cool animations
"Plug 'camspiers/animate.vim'
"Plug 'camspiers/lens.vim'
Plug 'yuttie/comfortable-motion.vim'

call plug#end()


"__COLORS__"


"set true colors for term + vim
if has('termguicolors')
  set termguicolors
endif
if (has('nvim'))
  let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
endif

"Theme Info
set background=dark
""let g:material_style = 'deep ocean'
let g:material_style = 'palenight'
""let g:material_terminal_italics = 1
"""colorscheme moonlight
""colorscheme nord
""colorscheme doom-one

"fix bg
hi NORMAL guibg=#282c34

""material theme
let g:material_italic_comment = v:true
let g:material_italic_keywords = v:true
let g:material_italic_functions = v:true
let g:material_italic_variables = v:true
let g:material_contrast = v:false
let g:material_borders = v:false
colorscheme material

"enable syntax
syntax enable
"set 256 colors
set t_Co=256

"Neovide + gui
""set guifont=SFMono\ Nerd\ Font:h13
set guifont=FiraCode\ Nerd\ Font:h13
""set guifont=FiraCode\ Nerd\ Font,DejaVu\ Sans:h13
let g:neovide_cursor_antialiasing=v:true
let g:neovide_fullscreen=v:true
let g:neovide_refresh_rate=60
let g:neovide_keyboard_layout="qwerty"

let g:neovide_cursor_vfx_mode = "pixiedust"
let g:neovide_cursor_animation_length=0.13
let g:neovide_cursor_trail_length=0.8


"__VIM_SETTINGS__"


"numrow transparent, vert split line transparent.
highlight clear SignColumn
hi VertSplit ctermfg=1 ctermbg=NONE cterm=NONE
set fillchars+=vert:┊

"highlight current number
set number
set cursorline
highlight CursorLine cterm=NONE ctermbg=NONE ctermfg=NONE guibg=NONE guifg=NONE
hi clear CursorLine
"Reset cursorline everytime colorscheme updates because stupid vim
augroup CLClear
    autocmd! ColorScheme * hi clear CursorLine
augroup END

"remove empty line symbols
let &fcs='eob: '

"set tab to just expand spaces
set tabstop=4
set softtabstop=4
set expandtab

"show last command
set showcmd

"indent based on filetypes
filetype indent on
filetype plugin on

"only draw when you need to
set lazyredraw

"highlight matching characters + search stuff
set showmatch
set incsearch
set hlsearch

" Indents word-wrapped lines as much as the 'parent' line
set breakindent
set formatoptions=l
set lbr

"macos clipbard
set clipboard=unnamed
"remove insert from bottom left
set noshowmode
"always display tabline and bufferlie"
set laststatus=2
""set showtabline=2
set noswapfile

"set autoindent
set autoindent
set shiftwidth=4

"enable the mouse
set mouse=a

"history and syntax
set history=100
set synmaxcol=240

"let vim open hidden buffers
set hidden


"__VIM_BINDINGS__"


"basic autopair
inoremap " ""<left>
inoremap ( ()<left>
inoremap [ []<left>
inoremap { {}<left>
inoremap {<CR> {<CR>}<ESC>O
inoremap {;<CR> {<CR>};<ESC>O

"remove arrow keys from normal mode (forces to use hjkl)
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

"jk to exit instead
inoremap jk <esc>
cnoremap jk <C-C>
" Note: In command mode mappings to esc run the command for some odd
" historical vi compatibility reason. We use the alternate method of
" existing which is Ctrl-C

"move by visual line not actal line
nnoremap j gj
nnoremap k gk

"use semicolon as colon in normal mode
nnoremap ; :
"leader keys (space)
let mapleader=" "

"shortcuts to open config
nnoremap <leader>ev :vsp $MYVIMRC<CR>
nnoremap <leader>ez :vsp ~/.zshrc<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

"easymotion/hop"
nnoremap <leader>w :HopWord<CR>
nnoremap <leader>l :HopLine<CR>

"fuzzy stuff
nnoremap <leader>o :History<CR>
nnoremap <leader>p :call fzf#vim#files('.', {'options': '--prompt ""'})<CR>
nnoremap <leader>f :BLines<CR>

"F12 for cool mode"
nnoremap <leader>z :TZAtaraxis<CR>

"save session
nnoremap <leader>s :mksession<CR>

"clear search highlight
nnoremap <silent> <leader>c :nohl<CR>

"reload init.vim without restart
map \r :source ~/.config/nvim/init.vim<CR>

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

"tell vim backups to go in /tmp
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup


"__LIGHTLINE, FZF, TERMINAL__"


"lightline setup
let g:lightline = {
    \ 'colorscheme': 'material',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             ['gitbranch', 'filetype', 'filename', 'wordcount', 'modified', 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ] ],
    \   'right': [ ['fileformat'],
    \              [ 'readonly', 'percent', 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ]]
    \ },
    \ 'component_function': {
    \    'filetype': 'MyFiletype',
    \    'fileformat': 'MyFileformat',
    \    'wordcount': 'WordCount',
    \    'gitbranch': 'FugitiveHead',
    \    'readonly': 'LightlineReadonly',
    \    'percent' : 'ScrollStatus',
    \ },
    \ 'component_expand': {
    \   'linter_checking': 'lightline#ale#checking',
    \   'linter_infos': 'lightline#ale#infos',
    \   'linter_warnings': 'lightline#ale#warnings',
    \   'linter_errors': 'lightline#ale#errors',
    \   'linter_ok': 'lightline#ale#ok',
    \ },
    \ 'component_type': {
    \   'linter_checking': 'right',
    \   'linter_infos': 'right',
    \   'linter_warnings': 'warning',
    \   'linter_errors': 'error',
    \   'linter_ok': 'right',
    \ },
    \ 'separator': { 'left': '', 'right': '' },
    \ 'subseparator': { 'left': '', 'right': '' }
    \ }

"add devicon to lightline
function! MyFiletype()
    return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
endfunction
function! MyFileformat()
    return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
endfunction

"create function to get read only
function! LightlineReadonly()
  return &readonly && &filetype !=# 'help' ? 'RO' : ''
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


let $FZF_DEFAULT_COMMAND =  "find * -path '*/\.*' -prune -o -path 'node_modules/**' -prune -o -path 'target/**' -prune -o -path 'dist/**' -prune -o  -type f -print -o -type l -print 2> /dev/null"

"doom
""""let $FZF_DEFAULT_OPTS=' --color=dark --color=fg:#bbc2cf,bg:#3c4557,hl:#baacff,fg+:#bbc2cf,bg+:#3c4557,hl+:#5B6268 --color=info:#3c4557,prompt:#3c4557,pointer:#c678dd,marker:#3c4557,spinner:#3c4557,header:-1 --layout=reverse  --margin=1,4'

"nord
""let $FZF_DEFAULT_OPTS=' --color=dark --color=fg:#bbc2cf,bg:#414C60,hl:#baacff,fg+:#bbc2cf,bg+:#414C60,hl+:#5B6268 --color=info:#414C60,prompt:#414C60,pointer:#c678dd,marker:#414C60,spinner:#414C60,header:-1 --layout=reverse  --margin=1,4'

"material ocean
""let $FZF_DEFAULT_OPTS=' --color=dark --color=fg:#bbc2cf,bg:#10141c,hl:#baacff,fg+:#bbc2cf,bg+:#10141c,hl+:#5B6268 --color=info:#10141c,prompt:#10141c,pointer:#c678dd,marker:#10141c,spinner:#10141c,header:-1 --layout=reverse  --margin=1,4'

"material palenight
""let $FZF_DEFAULT_OPTS=' --color=dark --color=fg:#bbc2cf,bg:#282d3f,hl:#baacff,fg+:#bbc2cf,bg+:#282d3f,hl+:#5B6268 --color=info:#282d3f,prompt:#282d3f,pointer:#c678dd,marker:#282d3f,spinner:#282d3f,header:-1 --layout=reverse  --margin=1,4'

"custom
let $FZF_DEFAULT_OPTS=' --color=dark --color=fg:#bbc2cf,bg:#2f334d,hl:#baacff,fg+:#bbc2cf,bg+:#2f334d,hl+:#5B6268 --color=info:#2f334d,prompt:#2f334d,pointer:#c678dd,marker:#2f334d,spinner:#2f334d,header:-1 --layout=reverse  --margin=1,4'

let g:fzf_layout = { 'window': 'call FloatingFZF()' }

""function to create a floating fzf window
function! FloatingFZF()
  let buf = nvim_create_buf(v:false, v:true)
  call setbufvar(buf, '&signcolumn', 'no')

  let height = float2nr(30)
  let width = float2nr(135)
  let horizontal = float2nr((&columns - width) / 2)
  ""let vertical = 1
  let vertical = float2nr((&lines - height) / 2)

  let opts = {
        \ 'relative': 'editor',
        \ 'row': vertical,
        \ 'col': horizontal,
        \ 'width': width,
        \ 'height': height,
        \ 'style': 'minimal'
        \ }

  call nvim_open_win(buf, v:true, opts)
endfunction

" Floating Term
function! FloatTerm(...)
  " Configuration
  let buf = nvim_create_buf(v:false, v:true)
  call setbufvar(buf, '&signcolumn', 'no')

  let height = float2nr(30)
  let width = float2nr(135)
  let horizontal = float2nr((&columns - width) / 2)
  ""let vertical = 1
  let vertical = float2nr((&lines - height) / 2)

  let opts = {
        \ 'relative': 'editor',
        \ 'row': vertical,
        \ 'col': horizontal,
        \ 'width': width,
        \ 'height': height,
        \ 'style': 'minimal',
        \ }

  let s:float_term_win = nvim_open_win(buf, v:true, opts)
  " Styling
  if a:0 == 0
    terminal
  else
    call termopen(a:1)
  endif
  startinsert
  autocmd TermClose * ++once :bd! | call nvim_win_close(s:float_term_win, v:true)
endfunction
"bindings
nnoremap <Leader>at :call FloatTerm()<CR>


"__PLUGIN_SETTINGS__"


"lightline ale
let g:lightline#ale#indicator_checking = "\uf110 "
let g:lightline#ale#indicator_infos = "\uf129 "
let g:lightline#ale#indicator_warnings = "\uf071 "
let g:lightline#ale#indicator_errors = "\uf05e "
let g:lightline#ale#indicator_ok = "\uf00c "

"disable ale on start
""let g:ale_enabled = 0
let g:ale_sign_error = '×'
let g:ale_sign_warning = '»'
""let g:ale_sign_error = '✘'
""let g:ale_sign_warning = '⚠'

let g:ale_linters = {
            \   'mail': ['proselint'],
            \   'markdown': ['proselint', 'languagetool'],
            \   'text': ['proselint', 'languagetool'],
    	    \   'python': ['pyls', 'autoimport', 'flake8', 'yapf'],
            \   }
let g:ale_fixers = {
\   '*':          ['remove_trailing_lines', 'trim_whitespace'],
\}

let g:ale_lint_on_text_changed = 'never'
let g:ale_fix_on_save = 1
let g:ale_lint_on_save = 1

"litecorrect
augroup litecorrect
  autocmd!
  autocmd FileType markdown,mkd call litecorrect#init()
  autocmd FileType textile call litecorrect#init()
augroup END

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
let g:indentLine_char = '┆'
""let g:indentLine_char_list = ['|', '¦', '┆', '┊']
let g:indentLine_setColors = 0
let g:indentLine_fileTypeExclude = ['dashboard'] "stop indentlines on dashboard

"NERDTree
"enable icons
let g:webdevicons_enable = 1
let g:webdevicons_enable_nerdtree = 1
let g:webdevicons_conceal_nerdtree_brackets = 1
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1
let g:NERDTreeHighlightCursorline = 0
"better ui
let NERDTreeMinimalUI=1
let NERDTreeDirArrows=1
let g:NERDTreeDirArrowExpandable = '»'
let g:NERDTreeDirArrowCollapsible = '«'
"let me see dotfiles
let NERDTreeShowHidden=1

"dashboard (use fzf + doom logo)
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
 `''                                                                      ''`
END

"minimap
let g:minimap_width = 10
let g:minimap_auto_start = 1
let g:minimap_auto_start_win_enter = 1

"limelight
""let g:limelight_default_coefficient = 0.7
let g:limelight_paragraph_span = 0

"discord presence
"let g:presence_auto_update       = 1
"let g:presence_editing_text      = "Editing %s"
"let g:presence_workspace_text    = "Working on %s"
"let g:presence_neovim_image_text = "The One True Text Editor"
"let g:presence_main_image        = "neovim"
"let g:presence_client_id         = "793271441293967371"
"let g:presence_debounce_timeout  = 15

"Vim multi-cursors
"use alt instead of ctrl
let g:VM_maps = {}
let g:VM_default_mappings = 0
let g:VM_maps["Add Cursor Down"]             = '<A-j>'
let g:VM_maps["Add Cursor Up"]               = '<A-k>'

"vim markdown
let g:vim_markdown_folding_disabled = 1
"quote stuff (curly instead of normal "", qc to autocorrect)
filetype plugin on       " may already be in your .vimrc
nnoremap <silent> qr <Plug>ReplaceWithCurly
"spell check for only markdown
autocmd FileType markdown setlocal spell
"set to conceal formatting by default to not clutter
""set conceallevel=2

augroup textobj_quote
  autocmd!
  autocmd FileType markdown call textobj#quote#init()
  autocmd FileType textile call textobj#quote#init()
  autocmd FileType text call textobj#quote#init({'educate': 0})
augroup END

"lazy load vim-fuigitive
command! Gstatus call LazyLoadFugitive('Gstatus')
command! Gdiff call LazyLoadFugitive('Gdiff')
command! Glog call LazyLoadFugitive('Glog')
command! Gblame call LazyLoadFugitive('Gblame')
function! LazyLoadFugitive(cmd)
  call plug#load('vim-fugitive')
  call fugitive#detect(expand('%:p'))
  exe a:cmd
endfunction

"limelight
let g:limelight_priority = -1
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!


"__LUA__"


"zen-mode settings
lua << EOF
-- setup for TrueZen.nvim
require("true-zen").setup({
    true_false_commands = false,
	cursor_by_mode = false,
	before_minimalist_mode_shown = false,
	before_minimalist_mode_hidden = false,
	after_minimalist_mode_shown = false,
	after_minimalist_mode_hidden = false,
	bottom = {
		hidden_laststatus = 0,
		hidden_ruler = false,
		hidden_showmode = false,
		hidden_showcmd = false,
		hidden_cmdheight = 1,

		shown_laststatus = 2,
		shown_ruler = true,
		shown_showmode = false,
		shown_showcmd = false,
		shown_cmdheight = 1
	},
	top = {
		hidden_showtabline = 0,

		shown_showtabline = 2
	},
	left = {
		hidden_number = false,
		hidden_relativenumber = false,
		hidden_signcolumn = "no",

		shown_number = true,
		shown_relativenumber = false,
		shown_signcolumn = "no"
	},
	ataraxis = {
		just_do_it_for_me = false,
		left_padding = 40,
		right_padding = 40,
		top_padding = 0,
		bottom_padding = 0,
		custome_bg = "#282c34"
	},
	focus = {
		margin_of_error = 5
	},
	integrations = {
		integration_galaxyline = false,
		integration_vim_airline = false,
		integration_vim_powerline = false,
		integration_tmux = false,
		integration_express_line = false,
		integration_gitgutter = false,
		integration_vim_signify = false,
		integration_limelight = false
	}
})
EOF

"colorizer
lua << EOF
require 'colorizer'.setup {
  '*' -- Highlight all files, but customize some others.
}
EOF
