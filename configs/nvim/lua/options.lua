-- Do not source the default filetype.vim
vim.g.did_load_filetypes = 1

vim.g.neovide_cursor_vfx_mode = "pixiedust"
vim.g.neovide_remember_window_size = true
vim.g.neovide_cursor_antialiasing = true

vim.opt.fillchars = { eob = " " }
vim.opt.undofile = true
vim.opt.swapfile = false
vim.opt.ignorecase = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.mouse = "a"
vim.opt.signcolumn = "yes"
vim.opt.clipboard = "unnamedplus"
vim.opt.scrolloff = 3
vim.opt.lazyredraw = true
vim.opt.linebreak = true
vim.opt.number = false
vim.opt.numberwidth = 2
vim.opt.shortmess:append "casI"
vim.opt.whichwrap:append "<>hl"
vim.opt.guifont = "Liga SFMono Nerd Font:h14"
