--sumneko likes to be annoying about things sometimes
---@diagnostic disable: undefined-global

--Install packer
local execute = vim.api.nvim_command
local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '.. install_path)
end
--setup packer
require('packer').startup(function()

  --packer can manage itself
  use "wbthomason/packer.nvim"

  --profiling
  use 'dstein64/vim-startuptime'

  --statusline, tabline, icons, tree, startmenu
  use {'hoob3rt/lualine.nvim', requires = {'kyazdani42/nvim-web-devicons', opt = true}}
  use {'akinsho/nvim-bufferline.lua', requires = 'kyazdani42/nvim-web-devicons'}
  use 'kyazdani42/nvim-tree.lua'
  use 'glepnir/dashboard-nvim'

  --git
  use { 'TimUntersberger/neogit', requires = 'nvim-lua/plenary.nvim' }
  use { 'lewis6991/gitsigns.nvim', requires = 'nvim-lua/plenary.nvim'}

  --distraction free and preview
  use 'kdav5758/TrueZen.nvim'
  use 'junegunn/limelight.vim'
  use 'norcalli/nvim-colorizer.lua'
  use 'junegunn/goyo.vim'

  --themes and syntax
  use 'shaunsingh/nord.nvim'
  --use 'shaunsingh/moonlight.nvim'
  --use 'shaunsingh/solarized.nvim'
  --use 'shaunsingh/seoul256.nvim'

  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
  use {"lukas-reineke/indent-blankline.nvim", branch = "lua"}
  use 'nvim-treesitter/playground'

  --easy motions
  use 'phaazon/hop.nvim'
  use 'ethanholz/nvim-lastplace'

  --fzf
  use {'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}}

  --completion
  use 'hrsh7th/nvim-compe'
  use 'onsails/lspkind-nvim'
  use 'neovim/nvim-lspconfig'
  use 'folke/lsp-trouble.nvim'
  use 'glepnir/lspsaga.nvim'
  use 'kabouzeid/nvim-lspinstall'
  use 'ray-x/lsp_signature.nvim'
  use {"ahmedkhalf/lsp-rooter.nvim"}

  --snippets, brackets,
  use 'jiangmiao/auto-pairs'
  use 'hrsh7th/vim-vsnip'
  use 'hrsh7th/vim-vsnip-integ'
  use 'rafamadriz/friendly-snippets'

  --which key for neovim
  use 'folke/which-key.nvim'

  --neovim everywhere
  use {
    "glacambre/firenvim",
    run = function()
      vim.fn["firenvim#install"](0)
    end,
  }

end)
--make life easier
local cmd = vim.cmd
local g = vim.g

--gui
g.neovide_fullscreen = true
--g.neovide_cursor_antialiasing=true
--g.neovide_cursor_vfx_mode = "pixiedust"
--neovide_cursor_animation_length = 0.05,
vim.api.nvim_exec([[set guifont=FiraCode\ Nerd\ Font:h14]], false)
--;vim.api.nvim_exec([[set guifont=SFMono\ Nerd\ Font:h14,SF\ Mono:h14,Liga\ Menlo:h14]], false)

--uivonim
  if vim.g.uivonim == 1 then
    local lsp_callbacks = require'uivonim.lsp'.callbacks
    nvim_lsp.texlab.setup {
      handlers = lsp_callbacks;
      on_attach = require('completion').on_attach
    }
    nvim_lsp.tsserver.setup {
      callbacks = lsp_callbacks;
    }
  end

--Hide eob~
vim.api.nvim_exec([[let &fcs='eob: ']], false)

--moonlight
--g.moonlight_style = "moonlight"
--g.moonlight_borders = true
--g.moonlight_contrast = false
--require('moonlight').set()

--solarized
--g.solarized_style = "solarized"
--g.solarized_borders = false
--g.solarized_contrast = false
--require('solarized').set()

--seoul256
--g.seoul256_style = "seoul256"
--g.seoul256_borders = false
--g.seoul256_contrast = false
--require('seoul256').set()

--flatwhite testing
--g.flatwhite_style = "flatwhite"
--g.flatwhite_borders = false
--g.flatwhite_contrast = false
--require('flatwhite').set()

--nord
g.nord_style = "nord"
g.nord_borders = false
g.nord_contrast = false
g.nord_cursorline_transparent = true
g.nord_disable_background = false -- doesn't work with neovide
require('nord').set()

--settings
local scopes = {o = vim.o, b = vim.bo, w = vim.wo}

local function opt(scope, key, value)
  scopes[scope][key] = value
  if scope ~= 'o' then scopes['o'][key] = value end
end

local indent = 4
cmd 'hi normal guibg=#2f334d'
opt('b', 'expandtab', true)                           -- use spaces instead of tabs
opt('b', 'shiftwidth', indent)                        -- Size of an indent
opt('b', 'smartindent', true)                         -- Insert indents automatically
opt('b', 'tabstop', indent)                           -- Number of spaces tabs count for
opt('o', 'inccommand', 'nosplit')
opt('o', 'cursorline', true)
opt('o', 'completeopt', 'menuone,noselect')           -- Completion options (for compe)
opt('o', 'hidden', true)                              -- Enable modified buffers in background
opt('o', 'scrolloff', 3 )                             -- Lines of context
opt('o', 'shiftround', true)                          -- Round indent
opt('o', 'sidescrolloff', 8 )                         -- Columns of context
opt('o', 'smartcase', true)                           -- Don't ignore case with capitals
opt('o', 'splitbelow', true)                          -- Put new windows below current
opt('o', 'splitright', true)                          -- Put new windows right of current
opt('o', 'termguicolors', true)                       -- True color support
opt('o', 'wildmode', 'list:longest')                  -- Command-line completion mode
opt('o', 'clipboard', 'unnamed')
opt('o', 'pumblend', 25 )
opt('o', 'scrolloff', 2 )
opt('o', 'swapfile', false )
opt('o', 'showmode', false )
opt('o', 'background', 'dark' )
opt('o', 'backup', false )
opt('o', 'updatetime', 100)
opt('w', 'number', true)                              -- Print line number
--opt('w', 'relativenumber', true)                              -- Print line number
opt('o', 'lazyredraw', true)
opt('o', 'signcolumn', 'yes')
opt('o', 'mouse', 'a')
opt('o', 'cmdheight', 1)
--opt('o', 'spell', true)
--opt('o', 'wrap', false)
opt('o', 'breakindent', true)
opt('o', 'lbr', true)
opt('o', 'formatoptions', 'l')
opt('o', 'pumheight', 10)

--mappings
local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

g.mapleader = " "                                                     --leader
map('i', 'jk', '<esc>')                                               --jk to exit
map('c', 'jk', '<C-C>')
map('n', ';', ':')                                                     --semicolon to enter command mode
map('n', 'j', 'gj')                                                    --move by visual line not actual line
map('n', 'k', 'gk')
map('n', '<leader>w', '<cmd>HopWord<CR>')                              --easymotion/hop
map('n', '<leader>l', '<cmd>HopLine<CR>')
map('n', '<leader>/', '<cmd>HopPattern<CR>')
map('n', '<leader>fr', '<cmd>Telescope oldfiles<CR>')                   --fuzzy
map('n', '<leader><space>', '<cmd>Telescope find_files<CR>')
map('n', '<leader><S-f>', '<cmd>Telescope current_buffer_fuzzy_find<CR>')
map('n', '<leader>fP', '<cmd>e ~/.config/nvim/init.lua<CR>')
map('n', '<leader>f', '<cmd>Telescope treesitter<CR>')
map('n', '<leader><S-p>', '<cmd>Telescope commands<CR>')
map('n', '<leader>z', '<cmd>TZAtaraxis<CR>')                           --ataraxis
map('n', '<leader><S-z>', '<cmd>TZAtaraxis l15 r15 t2 b2<CR>')
map('n', '<leader>x', '<cmd>Goyo<CR>')
map('n', '<leader>op', '<cmd>NvimTreeToggle<CR>')                      --nvimtree
map('n', '<c-k>', '<cmd>wincmd k<CR>')                                 --ctrlhjkl to navigate splits
map('n', '<c-j>', '<cmd>wincmd j<CR>')
map('n', '<c-h>', '<cmd>wincmd h<CR>')
map('n', '<c-l>', '<cmd>wincmd l<CR>')

cmd([[autocmd BufWritePre * %s/\s\+$//e]])                             --remove trailing whitespaces
cmd([[autocmd BufWritePre * %s/\n\+\%$//e]])

vim.api.nvim_set_keymap('n', '0', "getline('.')[0 : col('.') - 2] =~# '^\\s\\+$' ? '0' : '^'", {silent = true, noremap = true, expr = true}) --toggle between 0 and ^

--lspcomplete binds
vim.api.nvim_set_keymap('i', '<C-Space>', [[compe#complete()]], { noremap = true, silent = true, expr = true })
vim.api.nvim_set_keymap('i', '<CR>', [[compe#confirm('<CR>')]], { noremap = true, silent = true, expr = true })
vim.api.nvim_set_keymap('i', '<C-e>', [[compe#close('<C-e>')]], { noremap = true, silent = true, expr = true })
vim.api.nvim_set_keymap('i', '<C-f>', [[compe#scroll({ 'delta': +4 })]], { noremap = true, silent = true, expr = true })
vim.api.nvim_set_keymap('i', '<C-d>', [[compe#scroll({ 'delta': -4 })]], { noremap = true, silent = true, expr = true })

--lspsaga binds
map('n', '<leader>h', '<cmd>Lspsaga lsp_finder<CR>')
map('n', '<leader>a', '<cmd>Lspsaga code_action<CR>')
map('v', '<leader>a', '<cmd><C-U>Lspsaga range_code_action<CR>')
map('n', '<leader>K', '<cmd>Lspsaga hover_doc<CR>')
map('n', '<leader>s', '<cmd>Lspsaga signature_help<CR>')
map('n', '<leader>r', '<cmd>Lspsaga rename<CR>')
map('n', '<leader>d', '<cmd>Lspsaga show_line_diagnostics<CR>')

--f5 to run
vim.api.nvim_exec([[
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
]], false)

--bufferline
local bg = "#2e3440"
local bg2 = "#3b4252"
local bg3 = "#282c34"
local fg = "#CACed6"
local accent = "#81a1c1"
local accent2 = "#BF616A" -- Not saved
local accent3 = "#EBCB8B" -- Not saved
 require('bufferline').setup {
  options = {
    numbers = "none",
    mappings = true,
    -- NOTE: this plugin is designed with this icon in mind,
    -- and so changing this is NOT recommended, this is intended
    -- as an escape hatch for people who cannot bear it for whatever reason
    indicator_icon = '▎',
    buffer_close_icon = '',
    modified_icon = '●',
    close_icon = '',
    left_trunc_marker = '',
    right_trunc_marker = '',
    max_name_length = 18,
    max_prefix_length = 15, -- prefix used when a buffer is de-duplicated
    tab_size = 18,
    offsets = {
      {
          filetype = "NvimTree",
          text = "Files"
      }
    },
    show_buffer_icons = true, -- disable filetype icons for buffers
    show_buffer_close_icons = true,
    show_close_icon = false,
    show_tab_indicators = true,
    persist_buffer_sort = false, -- whether or not custom sorted buffers should persist
    -- can also be a table containing 2 custom separators
    -- [focused and unfocused]. eg: { '|', '|' }
    separator_style = "thin",
    enforce_regular_tabs = false,
    always_show_bufferline = true,
    sort_by = 'directory'
    },
    highlights = {
         fill = {
             guibg = bg
         },
        background = {
            guibg = bg
        },

        -- buffer
        buffer_selected = {
            guifg = fg,
            guibg = bg2,
            gui = "bold"
        },
        separator = {
            guifg = bg3,
            guibg = bg
        },
        separator_selected = {
            guifg = bg3,
            guibg = bg2
        },
        separator_visible = {
            guifg = bg2,
            guibg = bg2
        },
        indicator_selected = {
            guifg = accent,
            guibg = bg2
        },

        -- tabs over right
        tab = {
            guifg = fg,
            guibg = bg
        },
         tab_selected = {
            guifg = accent,
            guibg = bg2
        },
         tab_close = {
            guifg = accent,
            guibg = bg2
        },
        modified_selected = {
            guifg = accent2,
            guibg = bg2
        },
        modified = {
            guifg = accent3,
            guibg = bg
        },
        modified_visible = {
            guifg = accent,
            guibg = bg
        }
    }
}

--indentline
g.indentLine_enabled = 1
g.indent_blankline_char = "|"
g.indent_blankline_filetype_exclude = {"help", "terminal", "dashboard"}
g.indent_blankline_buftype_exclude = {"terminal"}
g.indent_blankline_show_trailing_blankline_indent = false
g.indent_blankline_show_first_indent_level = false
g.indent_blankline_use_treesitter = true

--terminal
vim.api.nvim_exec([[autocmd TermOpen * setlocal nonumber norelativenumber]], false)

--goyo
vim.api.nvim_exec([[
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!
]], false)

--nvim-compe
require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;

  source = {
    path = true;
    buffer = true;
    calc = true;
    spell = true;
    nvim_lsp = true;
    nvim_lua = true;
    vsnip = true;
  };
}


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
		hidden_showtabline = 0,

		shown_showtabline = 2
	},
	left = {
		hidden_number = false,
		hidden_relativenumber = false,
		hidden_signcolumn = "no",

		shown_number = true,
		shown_relativenumber = false,
		shown_signcolumn = "yes"
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
		force_hide_statusline = true
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

--nvim treesitter
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  highlight = {
    enable = true,
  },
  indent = {
    enable = true
  },
}

require "nvim-treesitter.configs".setup {
  playground = {
    enable = true,
    disable = {},
    updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
    persist_queries = false, -- Whether the query persists across vim sessions
    keybindings = {
      toggle_query_editor = 'o',
      toggle_hl_groups = 'i',
      toggle_injected_languages = 't',
      toggle_anonymous_nodes = 'a',
      toggle_language_display = 'I',
      focus_language = 'f',
      unfocus_language = 'F',
      update = 'R',
      goto_node = '<cr>',
      show_help = '?',
    },
  }
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
-- Snippets support
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
 vim.lsp.diagnostic.on_publish_diagnostics, {
   virtual_text = {
     prefix = " ", -- change this to whatever you want your diagnostic icons to be
   },
 }
)

-- Signature help
require('lsp_signature').on_attach()

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
        Enum = '',
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
require'lspconfig'.pyls.setup{
on_attach = function(client, bufnr)
    require "lsp_signature".on_attach()  -- Note: add in lsp client on-attach
  end,
}
local function setup_servers()
  require'lspinstall'.setup()
  local servers = require'lspinstall'.installed_servers()
  for _, server in pairs(servers) do
    require'lspconfig'[server].setup{}
    require "lsp_signature".on_attach()
  end
end

setup_servers()

-- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
require'lspinstall'.post_install_hook = function ()
  setup_servers() -- reload installed servers
  vim.cmd("bufdo e") -- this triggers the FileType autocmd that starts the server
end

require("trouble").setup {
    height = 7, -- height of the trouble list
    icons = true, -- use devicons for filenames
    mode = "lsp_workspace_diagnostics", -- "lsp_workspace_diagnostics", "lsp_document_diagnostics", "quickfix", "lsp_references", "loclist"
    fold_open = "", -- icon used for open folds
    fold_closed = "", -- icon used for closed folds
    action_keys = { -- key mappings for actions in the trouble list
        close = "q", -- close the list
        cancel = "<esc>", -- cancel the preview and get back to your last window / buffer / cursor
        refresh = "r", -- manually refresh
        jump = {"<cr>", "<tab>"}, -- jump to the diagnostic or open / close folds
        jump_close = {"o"}, -- jump to the diagnostic and close the list
        toggle_mode = "m", -- toggle between "workspace" and "document" diagnostics mode
        toggle_preview = "P", -- toggle auto_preview
        hover = "K", -- opens a small poup with the full multiline message
        preview = "p", -- preview the diagnostic location
        close_folds = {"zM", "zm"}, -- close all folds
        open_folds = {"zR", "zr"}, -- open all folds
        toggle_fold = {"zA", "za"}, -- toggle fold of current file
        previous = "k", -- preview item
        next = "j" -- next item
    },
    indent_lines = true, -- add an indent guide below the fold icons
    auto_open = false, -- automatically open the list when you have diagnostics
    auto_close = false, -- automatically close the list when you have no diagnostics
    auto_preview = true, -- automatyically preview the location of the diagnostic. <esc> to close preview and go back to last window
    auto_fold = false, -- automatically fold a file trouble list at creation
    signs = {
        -- icons / text used for a diagnostic
        error = "",
        warning = "",
        hint = "",
        information = "",
        other = "﫠"
    },
    use_lsp_diagnostic_signs = false
}

--lsptrobule bindings
vim.api.nvim_set_keymap("n", "<leader>gx", "<cmd>Trouble<cr>",  {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "<leader>gw", "<cmd>Trouble lsp_workspace_diagnostics<cr>", {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "<leader>gd", "<cmd>Trouble lsp_document_diagnostics<cr>", {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "<leader>gl", "<cmd>Trouble loclist<cr>", {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "<leader>gq", "<cmd>Trouble quickfix<cr>", {silent = true, noremap = true})
vim.api.nvim_set_keymap("n", "gR", "<cmd>Trouble lsp_references<cr>", {silent = true, noremap = true})

--whichkey
require("which-key").setup {
 plugins = {
    marks = true, -- shows a list of your marks on ' and `
    registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
    -- the presets plugin, adds help for a bunch of default keybindings in Neovim
    -- No actual key bindings are created
    spelling = {
      enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
      suggestions = 20, -- how many suggestions should be shown in the list?
    },
    presets = {
      operators = true, -- adds help for operators like d, y, ... and registers them for motion / text object completion
      motions = true, -- adds help for motions
      text_objects = true, -- help for text objects triggered after entering an operator
      windows = true, -- default bindings on <c-w>
      nav = true, -- misc bindings to work with windows
      z = true, -- bindings for folds, spelling and others prefixed with z
      g = true, -- bindings for prefixed with g
    },
  },
  -- add operators that will trigger motion and text object completion
  -- to enable all native operators, set the preset / operators plugin above
  operators = { gc = "Comments" },
  icons = {
    breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
    separator = "->", -- symbol used between a key and it's label
    group = "+", -- symbol prepended to a group
  },
  window = {
    border = "none", -- none, single, double, shadow
    position = "bottom", -- bottom, top
    margin = { 0, 1, 0, 1 }, -- extra window margin [top, right, bottom, left]
    padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
  },
  layout = {
    height = { min = 4, max = 25 }, -- min and max height of the columns
    width = { min = 20, max = 50 }, -- min and max width of the columns
    spacing = 3, -- spacing between columns
  },
  ignore_missing = false, -- enable this to hide mappings for which you didn't specify a label
  hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ "}, -- hide mapping boilerplate
  show_help = true, -- show help message on the command line when the popup is visible
  triggers = "auto", -- automatically setup triggers
  -- triggers = {"<leader>"} -- or specifiy a list manually
}

--lspsaga and icons
require'lspsaga'.init_lsp_saga{
    error_sign = "",
    warn_sign = "",
    hint_sign = "",
    infor_sign = "",
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

-- Use (s-)tab to move back
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

--neovim lastplace
require'nvim-lastplace'.setup{}

--evilline
local lualine = require'lualine'

-- Color table for highlights
--solarized
--local colors = {
--bg       = '#fdf6e3',
--fg       = '#002b36',
--yellow   = '#6c71c4',
--cyan     = '#7d8d09',
--orange   = '#cb4b16',
--violet   = '#b58900',
--magenta  = '#7d8d09',
--blue     = '#268bd2';
--red      = '#dc322f';
--}

--soul356
--local colors = {
--bg       = '#545454',
--fg       = '#dfe0e0',
--yellow   = '#c3a769',
--cyan     = '#c3a769',
--darkblue = '#d0a39f',
--green    = '#678568',
--orange   = '#67a9aa',
--violet   = '#84a4c1',
--magenta  = '#d0a39f',
--blue     = '#84a4c1';
--red      = '#a07474';
--}

--nord
local colors = {
 bg       = '#2E3440',
 fg       = '#ECEFF4',
 yellow   = '#EBCB8B',
 cyan     = '#8FBCBB',
 darkblue = '#5E81AC',
 green    = '#A3BE8C',
 orange   = '#D08770',
 violet   = '#81A1C1',
 magenta  = '#B48EAD',
 blue     = '#81A1C1';
 red      = '#BF616A';
}

local conditions = {
  buffer_not_empty = function()
    return vim.fn.empty(vim.fn.expand('%:t')) ~= 1
  end,
  hide_in_width = function()
    return vim.fn.winwidth(0) > 80
  end,
  check_git_workspace = function()
    local filepath = vim.fn.expand('%:p:h')
    local gitdir = vim.fn.finddir('.git', filepath .. ';')
    return gitdir and #gitdir > 0 and #gitdir < #filepath
  end
}

-- Config
local config = {
  options = {
    -- Disable sections and component separators
    component_separators = "",
    section_separators = "",
    theme = {
      -- We are going to use lualine_c and lualine_x as left and
      -- right section. Both are highlighted by c theme .  So we
      -- are just setting default looks o statusline
      normal = { c = {fg = colors.fg, bg = colors.bg}},
      inactive = { c = {fg = colors.fg, bg = colors.bg}}
    },
  },
  sections = {
    -- these are to remove the defaults
    lualine_a = {},
    lualine_b = {},
    lualine_y = {},
    lualine_z = {},
    -- These will be filled later
    lualine_c = {},
    lualine_x = {},
  },
  inactive_sections = {
    -- these are to remove the defaults
    lualine_a = {},
    lualine_v = {},
    lualine_y = {},
    lualine_z = {},
    lualine_c = {},
    lualine_x = {},
  }
}

-- Inserts a component in lualine_c at left section
local function ins_left(component)
  table.insert(config.sections.lualine_c, component)
end

-- Inserts a component in lualine_x ot right section
local function ins_right(component)
  table.insert(config.sections.lualine_x, component)
end

ins_left {
 function() return '▊' end,
 color = {fg = colors.blue}, -- Sets highlighting of component
 left_padding = 0 -- We don't need space before this
}

ins_left {
  -- mode component
  function()
    -- auto change color according to neovims mode
    local mode_color = {
      n      = colors.red,
      i      = colors.green,
      v      = colors.blue,
      [''] = colors.blue,
      V      = colors.blue,
      c      = colors.magenta,
      no     = colors.red,
      s      = colors.orange,
      S      = colors.orange,
      [''] = colors.orange,
      ic     = colors.yellow,
      R      = colors.violet,
      Rv     = colors.violet,
      cv     = colors.red,
      ce     = colors.red,
      r      = colors.cyan,
      rm     = colors.cyan,
      ['r?'] = colors.cyan,
      ['!']  = colors.red,
      t      = colors.red
    }
    vim.api.nvim_command('hi! LualineMode guifg='..mode_color[vim.fn.mode()] .. " guibg="..colors.bg)
    return ''
  end,
  color = "LualineMode",
  left_padding = 0,
}

ins_left {
  -- filesize component
  function()
    local function format_file_size(file)
      local size = vim.fn.getfsize(file)
      if size <= 0 then return '' end
      local sufixes = {'b', 'k', 'm', 'g'}
      local i = 1
      while size > 1024 do
        size = size / 1024
        i = i + 1
      end
      return string.format('%.1f%s', size, sufixes[i])
    end
    local file = vim.fn.expand('%:p')
    if string.len(file) == 0 then return '' end
    return format_file_size(file)
  end,
  condition = conditions.buffer_not_empty,
}

ins_left {
'filetype', format = function() return " " end, right_padding=0
}

ins_left {
  'filename',
  condition = conditions.buffer_not_empty,
  path = 1,
  color = {fg = colors.yellow, gui = 'bold'},
  left_padding=0
}

ins_left {
  'location',
  color = {fg = colors.darkblue, gui = 'bold'}
}

ins_left {
  'progress',
  color = {fg = colors.darkblue, gui = 'bold'},
}

ins_left {
  'diagnostics',
  sources = {'nvim_lsp'},
  symbols = {error = ' ', warn = ' ', info= ' '},
  color_error = colors.red,
  color_warn = colors.yellow,
  color_info = colors.cyan,
}

ins_right {
  -- Lsp server name .
  function ()
    local msg = 'none'
    local buf_ft = vim.api.nvim_buf_get_option(0,'filetype')
    local clients = vim.lsp.get_active_clients()
    if next(clients) == nil then return msg end
    for _, client in ipairs(clients) do
      local filetypes = client.config.filetypes
      if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
        return client.name
      end
    end
    return msg
  end,
  icon = '[LSP]',
  color = {fg = colors.fg, gui = 'bold'}
}

ins_right {
  function() return 'LF' end,
  color = {fg = colors.darkblue},
}
ins_right {
  'fileformat', --same one just without the logo
  icons_enabled = true,
  color = {fg = colors.darkblue, gui='bold'},
}

ins_right {
  'o:encoding', -- option component same as &encoding in viml
  upper = true, -- I'm not sure why it's upper case either ;)
  condition = conditions.hide_in_width,
  color = {fg = colors.darkblue, gui = 'bold'}
}

ins_right {
  'branch',
  icon = '',
  condition = conditions.check_git_workspace,
  color = {fg = colors.green, gui = 'bold'},
}

ins_right {
  'diff',
  -- Is it me or the symbol for modified us really weird
  symbols = {added= ' ', modified= ' ', removed= ' '},
  color_added = colors.green,
  color_modified = colors.orange,
  color_removed = colors.red,
  condition = conditions.hide_in_width
}

ins_right {
  function() return '▊' end,
  color = {fg = colors.blue},
  right_padding = 0,
}
lualine.setup(config)

--colorizer
require'colorizer'.setup()

--nvimtree
g.nvim_tree_side = "left"
g.nvim_tree_width = 25
g.nvim_tree_ignore = {".git", "node_modules", ".cache"}
g.nvim_tree_auto_open = 0
g.nvim_tree_auto_close = 0
g.nvim_tree_quit_on_open = 0
g.nvim_tree_follow = 1
g.nvim_tree_indent_markers = 1
g.nvim_tree_hide_dotfiles = 1
g.nvim_tree_git_hl = 1
g.nvim_tree_root_folder_modifier = ":~"
g.nvim_tree_allow_resize = 1

g.nvim_tree_show_icons = {
    git = 1,
    folders = 1,
    files = 1
}

g.nvim_tree_icons = {
    default = '',
    symlink = '',
    git  = {
      unstaged = "",
      staged = "✓",
      unmerged = "",
      renamed = "",
      untracked = "",
      deleted = "",
      ignored = ""
      },
    folder  = {
      default = "",
      open = "",
      empty = "",
      empty_open = "",
      symlink = "",
      symlink_open = "",
      },
      lsp  = {
        hint = "",
        info = "",
        warning = "",
        error = "",
        }
}

--telescope
local previewers = require('telescope.previewers')
local putils = require('telescope.previewers.utils')
local pfiletype = require('plenary.filetype')

local new_maker = function(filepath, bufnr, opts)
  opts = opts or {}
  if opts.use_ft_detect == nil then
    local ft = pfiletype.detect(filepath)
    -- Here for example you can say: if ft == "xyz" then this_regex_highlighing else nothing end
    opts.use_ft_detect = false
    putils.regex_highlighter(bufnr, ft)
  end
  previewers.buffer_previewer_maker(filepath, bufnr, opts)
end

require('telescope').setup{
  defaults = {
    vimgrep_arguments = {
      'rg',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case'
    },
    prompt_position = "top",
    prompt_prefix = " ",
    selection_caret = " ",
    entry_prefix = "  ",
    initial_mode = "insert",
    selection_strategy = "reset",
    sorting_strategy = "ascending",
    layout_strategy = "vertical",
    layout_defaults = {
      horizontal = {
        mirror = false,
      },
      vertical = {
        mirror = true,
      },
    },
    file_sorter =  require'telescope.sorters'.get_fuzzy_file,
    file_ignore_patterns = {},
    generic_sorter =  require'telescope.sorters'.get_generic_fuzzy_sorter,
    shorten_path = true,
    winblend = 0,
    width = 0.75,
    preview_cutoff = 120,
    results_height = 1,
    results_width = 0.8,
    border = {},
    borderchars = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
    color_devicons = true,
    use_less = true,
    set_env = { ['COLORTERM'] = 'truecolor' }, -- default = nil,
    file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
    grep_previewer = require'telescope.previewers'.vim_buffer_vimgrep.new,
    qflist_previewer = require'telescope.previewers'.vim_buffer_qflist.new,

    -- Developer configurations: Not meant for general override
    buffer_previewer_maker = new_maker,
  },
}


vim.g.dashboard_session_directory = '~/.config/nvim/.sessions'
vim.g.dashboard_default_executive = 'telescope'
vim.cmd("let g:dashboard_default_executive = 'telescope'")

vim.cmd("let g:dashboard_session_directory = $HOME..'/.config/nvim/.sessions'")
vim.cmd("let packages = len(globpath('~/.local/share/nvim/site/pack/packer/start', '*', 0, 1))")

vim.api.nvim_exec([[
    let g:dashboard_custom_footer = ['LuaJIT loaded '..packages..' packages']
]], false)

vim.g.dashboard_custom_section = {
  a = {description = {'DOOM - Its EVIL'}, command = 'qa!'},
}

--vim.g.dashboard_custom_header = {
       --"                                                                    ",
       --"                                                                    ",
       --"                                                                    ",
       --"                                                                    ",
       --"            :h-                                  Nhy`               ",
       --"           -mh.                           h.    `Ndho               ",
       --"           hmh+                          oNm.   oNdhh               ",
       --"          `Nmhd`                        /NNmd  /NNhhd               ",
       --"          -NNhhy                      `hMNmmm`+NNdhhh               ",
       --"          .NNmhhs              ```....`..-:/./mNdhhh+               ",
       --"           mNNdhhh-     `.-::///+++////++//:--.`-/sd`               ",
       --"           oNNNdhhdo..://++//++++++/+++//++///++/-.`                ",
       --"      y.   `mNNNmhhhdy+/++++//+/////++//+++///++////-` `/oos:       ",
       --" .    Nmy:  :NNNNmhhhhdy+/++/+++///:.....--:////+++///:.`:s+        ",
       --" h-   dNmNmy oNNNNNdhhhhy:/+/+++/-         ---:/+++//++//.`         ",
       --" hd+` -NNNy`./dNNNNNhhhh+-://///   -+ooo:`  ::-:+////++///:`        ",
       --" /Nmhs+oss-:++/dNNNmhho:--::///   /mmmmmmo  ../-///++///////.       ",
       --"  oNNdhhhhhhhs//osso/:---:::///   /myyyyso  ..o+-//////////:/.      ",
       --"   /mNNNmdhhhh/://+///::://////     -:::- ..+sy+:////////::/:/.     ",
       --"     /hNNNdhhs--:/+++////++/////.      ..-/yhhs-/////////::/::/`    ",
       --"       .ooo+/-::::/+///////++++//-/ossyyhhhhs/:///////:::/::::/:    ",
       --"       -///:::::::////++///+++/////:/+ooo+/::///////.::://::---+`   ",
       --"       /////+//++++/////+////-..//////////::-:::--`.:///:---:::/:   ",
       --"       //+++//++++++////+++///::--                 .::::-------::   ",
       --"       :/++++///////////++++//////.                -:/:----::../-   ",
       --"       -/++++//++///+//////////////               .::::---:::-.+`   ",
       --"       `////////////////////////////:.            --::-----...-/    ",
       --"        -///://////////////////////::::-..      :-:-:-..-::.`.+`    ",
       --"         :/://///:///::://::://::::::/:::::::-:---::-.-....``/mm`   ",
       --"           ::::://::://::::::::::::::----------..-:....`.../Nmhd+o/ ",
       --"            -/:::-:::::---://:-::-::::----::---.-.......`-/oNN   `` ",
       --"           s-`::--:::------:////----:---.-:::...-.....`./:          ",
       --"          yMNy.`::-.--::..-dmmhhhs-..-.-.......`.....-/:`           ",
       --"         oMNNNh. `-::--...:NNNdhhh/.--.`..``.......:/-              ",
       --"        :dy+:`      .-::-..NNNhhd+``..`...````.-::-`                ",
       --"                        .-:mNdhh:.......--::::-`                    ",
       --"                           yNh/..------..`                          ",
--}

vim.g.dashboard_custom_header = {
                  "=================     ===============     ===============   ========  ========",
                  "\\\\ . . . . . . .\\\\   //. . . . . . .\\\\   //. . . . . . .\\\\  \\\\. . .\\\\// . . //",
                  "||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\\/ . . .||",
                  "|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||",
                  "||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||",
                  "|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\\ . . . . ||",
                  "||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\\_ . .|. .||",
                  "|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\\ `-_/| . ||",
                  "||_-' ||  .|/    || ||    \\|.  || `-_|| ||_-' ||  .|/    || ||   | \\  / |-_.||",
                  "||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \\  / |  `||",
                  "||    `'         || ||         `'    || ||    `'         || ||   | \\  / |   ||",
                  "||            .===' `===.         .==='.`===.         .===' /==. |  \\/  |   ||",
                  "||         .=='   \\_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \\/  |   ||",
                  "||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \\/  |   ||",
                  "||   .=='    _-'          `-__\\._-'         `-_./__-'         `' |. /|  |   ||",
                  "||.=='    _-'                                                     `' |  /==.||",
                  "=='    _-'                        N E O V I M                         \\/   `==",
                  "\\   _-'                                                                `-_   /",
                  " `''                                                                      ``'  ",
                  "                                                                               ",
}
