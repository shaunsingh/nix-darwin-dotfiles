set nocompatible


"__PLUGINS__"


"autoinstall vim plug
"check for uninstalled plugins
"let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
"if empty(glob(data_dir . '/autoload/plug.vim'))
"  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
"  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
"endif
""auto install plugins
"autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
"  \| PlugInstall --sync | source $MYVIMRC
"\| endif

call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')

"speed/profiling
Plug 'dstein64/vim-startuptime'

"statusline/bufferline he
Plug 'shaunsingh/lightline.vim' "has my moonlight theme
Plug 'shaunsingh/moonlight.nvim'

"smooth scroll and statusline
Plug 'yuttie/comfortable-motion.vim'
Plug 'ojroques/vim-scrollstatus'

"icons
Plug 'ryanoasis/vim-devicons'
Plug 'kyazdani42/nvim-web-devicons'

"fuzzy search + files
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } | Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' } | Plug 'tiagofumo/vim-nerdtree-syntax-highlight', { 'on': 'NERDTreeToggle' }

"minimap // enable when supported in openGL Neovide
""Plug 'wfxr/minimap.vim', {'do': ':!cargo install --locked code-minimap'}

"git
Plug 'nvim-lua/plenary.nvim' | Plug 'TimUntersberger/neogit' | Plug 'lewis6991/gitsigns.nvim'

"unix commands
"Plug 'tpope/vim-eunuch'

"start dash + give a tip
Plug 'glepnir/dashboard-nvim'

"distraction free/zen mode
Plug 'kdav5758/TrueZen.nvim'
Plug 'junegunn/goyo.vim', {'on': 'Goyo' } | Plug 'junegunn/limelight.vim'

"add color to hex
Plug 'norcalli/nvim-colorizer.lua'

"markdown writing
Plug 'kana/vim-textobj-user', {'for': 'markdown'} | Plug 'preservim/vim-textobj-quote', {'for': 'markdown'}
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }

"syntax
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

"lines on indents + auto pairs+ multiple cursors
Plug 'Yggdroot/indentLine' | Plug 'lukas-reineke/indent-blankline.nvim'
Plug 'mg979/vim-visual-multi', {'branch': 'master'}

"linting + lsp (ale for linting), (compe for completion, lspkind icons, tabnine, lspconfig for kotlin kotlin_language_server)
Plug 'dense-analysis/ale' | Plug 'maximbaz/lightline-ale'
Plug 'hrsh7th/nvim-compe' | Plug 'onsails/lspkind-nvim' | Plug 'tzachar/compe-tabnine', { 'do': './install.sh' } | Plug 'neovim/nvim-lspconfig' |
Plug 'folke/lsp-trouble.nvim' | Plug 'glepnir/lspsaga.nvim'
"rich presence
""Plug 'andweeb/presence.nvim'

"easymotions
Plug 'phaazon/hop.nvim'

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

"fix bg
hi NORMAL guibg=#2f334d

""material theme
let g:material_style = 'moonlight'
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
set guifont=FiraCode\ Nerd\ Font:h13
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
set noruler

"always display tabline and bufferline"
set laststatus=2
set showtabline=2
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

"clear search highlight
nnoremap <leader>c :nohl<CR>

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

"tell vim backups to not exist
set nobackup


"__LIGHTLINE, FZF, TERMINAL__"


"lightline setup
let g:lightline = {
    \ 'colorscheme': 'moonlight',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'filetype', 'filename', 'wordcount', 'modified', 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ] ],
    \   'right': [ [ 'fileformat' ],
    \              [ 'readonly', 'percent', 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ]]
    \ },
    \ 'component_function': {
    \    'filetype': 'MyFiletype',
    \    'fileformat': 'MyFileformat',
    \    'wordcount': 'WordCount',
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

"floating window specs
let $FZF_DEFAULT_OPTS=' --color=dark --color=fg:#bbc2cf,bg:#2f334d,hl:#baacff,fg+:#bbc2cf,bg+:#2f334d,hl+:#5B6268 --color=info:#2f334d,prompt:#2f334d,pointer:#c678dd,marker:#2f334d,spinner:#2f334d,header:-1 --layout=reverse  --margin=1,4'

let g:fzf_layout = { 'window': 'call FloatingFZF()' }

"semi transparent floating window and autocomplete
set winblend=13
set pumblend=15

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
let g:lightline#ale#indicator_warnings = " "
let g:lightline#ale#indicator_errors = " "
let g:lightline#ale#indicator_ok = "\uf00c "

"disable ale on start
""let g:ale_enabled = 0
let g:ale_sign_error = ""
let g:ale_sign_warning = ""

let g:ale_linters = {
            \   'mail': ['proselint'],
            \   'markdown': ['proselint'],
    	    \   'python': ['pyls', 'autoimport', 'flake8', 'yapf'],
            \   }
let g:ale_fixers = {
\   '*':          ['remove_trailing_lines', 'trim_whitespace'],
\}

let g:ale_lint_on_text_changed = 'never'
let g:ale_fix_on_save = 1
let g:ale_lint_on_save = 1

"Lsp
set completeopt=menuone,noselect
let g:compe = {}
let g:compe.enabled = v:true
let g:compe.autocomplete = v:true
let g:compe.debug = v:false
let g:compe.min_length = 1
let g:compe.preselect = 'enable'
let g:compe.throttle_time = 80
let g:compe.source_timeout = 200
let g:compe.incomplete_delay = 400
let g:compe.max_abbr_width = 100
let g:compe.max_kind_width = 100
let g:compe.max_menu_width = 100
let g:compe.documentation = v:true

let g:compe.source = {}
let g:compe.source.spell = v:true
let g:compe.source.path = v:true
let g:compe.source.buffer = v:true
let g:compe.source.calc = v:true
let g:compe.source.nvim_lsp = v:true
let g:compe.source.nvim_lua = v:true
let g:compe.source.tabnine = v:true


""let g:compe.source.treesitter = v:true


let g:compe.source.tabnine = {}
let g:compe.source.tabnine.max_line = 1000
let g:compe.source.tabnine.max_num_results = 6
let g:compe.source.tabnine.priority = 5000
" setting sort to false means compe will leave tabnine to sort the completion items
let g:compe.source.tabnine.sort = v:false
let g:compe.source.tabnine.show_prediction_strength = v:true
let g:compe.source.tabnine.ignore_pattern = ''

"compe mappings
inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')
inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })

"make cursor line -> block
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_SR = "\<Esc>]50;CursorShape=2\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"

"anti-delay for above
set ttimeout
set ttimeoutlen=1
set listchars=tab:>-,trail:~,extends:>,precedes:<,space:.
set ttyfast

"indentline
let g:indentLine_char = '|'
""let g:indentLine_char_list = ['|', '¦', '┆', '┊']
let g:indentLine_setColors = 0
let g:indentLine_fileTypeExclude = ['dashboard'] "stop indentlines on dashboard

"scrolling
noremap <silent> <ScrollWheelDown> :call comfortable_motion#flick(100)<CR>
noremap <silent> <ScrollWheelUp>   :call comfortable_motion#flick(-100)<CR>

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
let g:NERDTreeDirArrowExpandable = ' '
let g:NERDTreeDirArrowCollapsible = ' '
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
setlocal spelllang=en_us

augroup textobj_quote
  autocmd!
  autocmd FileType markdown call textobj#quote#init()
augroup END

""limelight
let g:limelight_priority = -1
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!


"__LUA__"


"zen-mode settings
lua << EOF
-- setup for TrueZen.nvim
local true_zen = require("true-zen")
true_zen.setup({
    true_false_commands = false,
	cursor_by_mode = false,
	bottom = {
		hidden_laststatus = 0,
		hidden_ruler = false,
		hidden_showmode = false,
		hidden_showcmd = false,
		hidden_cmdheight = 1,

		shown_laststatus = 2,
		shown_ruler = false,
		shown_showmode = false,
		shown_showcmd = false,
		shown_cmdheight = 1
	},
	top = {
		hidden_showtabline = 2,

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
		just_do_it_for_me = true,
		left_padding = 40,
		right_padding = 40,
		top_padding = 0,
		bottom_padding = 0,
		custome_bg = "#2f334d",
		disable_bg_configuration = false,
		disable_fillchars_configuration = false,
		force_when_plus_one_window = true,
		force_hide_statusline = false
	},
	focus = {
		margin_of_error = 5,
		focus_method = "experimental"
	},
	events = {
		before_minimalist_mode_shown = false,
		before_minimalist_mode_hidden = false,
		after_minimalist_mode_shown = false,
		after_minimalist_mode_hidden = false
	},
	integrations = {
		integration_galaxyline = false,
		integration_vim_airline = false,
		integration_vim_powerline = false,
		integration_tmux = false,
		integration_express_line = false,
		integration_gitgutter = false,
		integration_vim_signify = false,
		integration_limelight = false,
		integration_tzfocus_tzataraxis = true
	}
})

--colorizer setup
require 'colorizer'.setup {
  '*' -- Highlight all files, but customize some others.
}

--nvim treesitter
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  highlight = {
    enable = true,              -- false will disable the whole extension
  },
}

--neogit
local neogit = require('neogit')
neogit.setup {}

--gitsigns
require('gitsigns').setup {
  signs = {
    add          = {hl = 'GitSignsAdd'   , text = '│', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
    change       = {hl = 'GitSignsChange', text = '│', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
    delete       = {hl = 'GitSignsDelete', text = '_', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    topdelete    = {hl = 'GitSignsDelete', text = '‾', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    changedelete = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
  },
  numhl = false,
  linehl = false,
  keymaps = {
    -- Default keymap options
    noremap = true,
    buffer = true,

    ['n ]c'] = { expr = true, "&diff ? ']c' : '<cmd>lua require\"gitsigns\".next_hunk()<CR>'"},
    ['n [c'] = { expr = true, "&diff ? '[c' : '<cmd>lua require\"gitsigns\".prev_hunk()<CR>'"},

    ['n <leader>hs'] = '<cmd>lua require"gitsigns".stage_hunk()<CR>',
    ['n <leader>hu'] = '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
    ['n <leader>hr'] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
    ['n <leader>hR'] = '<cmd>lua require"gitsigns".reset_buffer()<CR>',
    ['n <leader>hp'] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
    ['n <leader>hb'] = '<cmd>lua require"gitsigns".blame_line()<CR>',

    -- Text objects
    ['o ih'] = ':<C-U>lua require"gitsigns".select_hunk()<CR>',
    ['x ih'] = ':<C-U>lua require"gitsigns".select_hunk()<CR>'
  },
  watch_index = {
    interval = 1000
  },
  current_line_blame = false,
  sign_priority = 6,
  update_debounce = 100,
  status_formatter = nil, -- Use default
  use_decoration_api = true,
  use_internal_diff = true,  -- If luajit is present
}

--lspkind for icons
require('lspkind').init({
     with_text = true,
     symbol_map = {
       Text = '',
       Method = 'ƒ',
       Function = '',
       Constructor = '',
       Variable = '',
       Class = '',
       Interface = 'ﰮ',
       Module = '',
       Property = '',
       Unit = '',
       Value = '',
       Enum = '了',
       Keyword = '',
       Snippet = '﬌',
       Color = '',
       File = '',
       Folder = '',
       EnumMember = '',
       Constant = '',
       Struct = ''
     },
})

--lspconfig + lsp trouble + lspsaga
require'lspconfig'.pyls.setup{}
require'lspconfig'.kotlin_language_server.setup{ cmd = { "/Users/shauryasingh/lsp/server/bin/kotlin-language-server" }}
require("trouble").setup {}
require'lspsaga'.init_lsp_saga{
    error_sign = "",
    warn_sign = "",
    hint_sign = "",
    infor_sign = ""
}

--use tab to navigate autocomplete
local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
        return true
    else
        return false
    end
end

-- Use (s-)tab to:
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder
_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-n>"
  elseif vim.fn.call("vsnip#available", {1}) == 1 then
    return t "<Plug>(vsnip-expand-or-jump)"
  elseif check_back_space() then
    return t "<Tab>"
  else
    return vim.fn['compe#complete']()
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  elseif vim.fn.call("vsnip#jumpable", {-1}) == 1 then
    return t "<Plug>(vsnip-jump-prev)"
  else
    return t "<S-Tab>"
  end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

EOF
