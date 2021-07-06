-- load all plugins
require "pluginList"
require "misc-utils"
require "top-bufferline"

local g = vim.g

g.mapleader = " "
g.auto_save = false

g.neovide_fullscreen = true
vim.api.nvim_exec([[set guifont=FiraCode\ Nerd\ Font:h14]], false)
vim.api.nvim_exec([[let &fcs='eob: ']], false)
--nord
g.nord_style = "nord"
g.nord_borders = false
g.nord_contrast = false
g.nord_cursorline_transparent = true
g.nord_disable_background = false -- doesn't work with neovide
require('nord').set()

require "mappings"
require "statusline"

-- hide line numbers , statusline in specific buffers!
vim.api.nvim_exec(
    [[
   au BufEnter term://* setlocal nonumber
   au BufEnter,BufWinEnter,WinEnter,CmdwinEnter * if bufname('%') == "NvimTree" | set laststatus=0 | else | set laststatus=2 | endif
   au BufEnter term://* set laststatus=0
]],
    false
)
