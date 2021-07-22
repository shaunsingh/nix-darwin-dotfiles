local opt = vim.opt
local g = vim.g

-- Turn these off at startup, will be enabled later just before loading the theme
vim.cmd([[
    syntax off
    filetype off
    filetype plugin indent off
]])

--don't write to the ShaDa file on startup
vim.opt.shadafile = "NONE"

--set termguicolors
vim.o.termguicolors = true

--fish slows things down
vim.opt.shell = "/bin/bash"

opt.ruler = false
opt.hidden = true
opt.ignorecase = true
opt.splitbelow = true
opt.splitright = true
opt.cul = true
opt.mouse = "a"
opt.signcolumn = "yes"
opt.cmdheight = 1
opt.updatetime = 250 -- update interval for gitsigns
opt.timeoutlen = 400
opt.clipboard = "unnamedplus"
opt.scrolloff = 3
opt.lazyredraw = true

-- Numbers
opt.number = false
opt.numberwidth = 2

-- for indentline
opt.expandtab = true
opt.shiftwidth = 4
opt.smartindent = true

-- shortmess options
opt.shortmess:append("asI") --disable intro

-- disable tilde on end of buffer:
vim.cmd("let &fcs='eob: '")

local disabled_built_ins = {
    "netrw",
    "netrwPlugin",
    "netrwSettings",
    "netrwFileHandlers",
    "gzip",
    "zip",
    "zipPlugin",
    "tar",
    "tarPlugin",
    "getscript",
    "getscriptPlugin",
    "vimball",
    "vimballPlugin",
    "2html_plugin",
    "logipat",
    "rrhelper",
    "spellfile_plugin",
    "matchit"
}

for _, plugin in pairs(disabled_built_ins) do
    vim.g["loaded_" .. plugin] = 1
end

--neovide
g.neovide_fullscreen = true
vim.o.guifont = "FiraCode Nerd Font:h15"
