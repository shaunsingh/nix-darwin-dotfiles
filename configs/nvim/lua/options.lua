-- Do not source the default filetype.vim
vim.g.did_load_filetypes = 1
vim.g.neovide_cursor_vfx_mode = "pixiedust" -- neovide trail
vim.opt.fillchars = { eob = " " } -- disable tilde fringe
vim.opt.undofile = true -- enable persistent undo
vim.opt.swapfile = false -- disable swap
vim.opt.ignorecase = true -- case insensitive search
vim.opt.splitbelow = true -- default to split below/right
vim.opt.splitright = true
vim.opt.cursorline = true -- enable cursorline
vim.opt.mouse = "a" -- enable mouse
vim.opt.signcolumn = "yes" -- enable signcolumn
vim.opt.clipboard = "unnamedplus" -- enable universal clipboard
vim.opt.scrolloff = 3 -- leave 3 lines up/down while scrolling
vim.opt.tabstop = 4 -- tabs should be 4 "space" wide
vim.opt.shiftwidth = 4 -- tabs should be 4 "space" wide
vim.opt.lazyredraw = true -- usefull for regexes with large files
vim.opt.linebreak = true -- clean linebreaks
vim.opt.number = false -- disable numbers
vim.opt.numberwidth = 2 -- two wide number column
vim.opt.shortmess:append "casI" -- disable intro
vim.opt.whichwrap:append "<>hl" -- clean aligned wraps
vim.opt.guifont = "Liga SFMono Nerd Font:h14" -- set guifont for neovide
