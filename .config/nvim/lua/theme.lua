local g = vim.g

g.mapleader = " "

--nord
g.nord_style = "nord"
g.nord_borders = false
g.nord_contrast = false
g.nord_cursorline_transparent = true
g.nord_disable_background = false -- doesn't work with neovide
require('nord').set()

vim.cmd([[
    syntax on
    filetype on
    filetype plugin indent on
]])


