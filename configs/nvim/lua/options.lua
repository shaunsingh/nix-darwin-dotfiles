--vim.g.neovide_fullscreen = true
--vim.g.neovide_cursor_vfx_mode = "ripple"
vim.g.neovide_cursor_vfx_mode = "pixiedust"

vim.opt.fillchars = { eob = " " }
vim.opt.termguicolors = true
vim.opt.undofile = true
vim.opt.swapfile = false
vim.opt.ignorecase = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.cursorline = true
vim.opt.mouse = "a"
vim.opt.signcolumn = "yes"
vim.opt.updatetime = 500
vim.opt.timeoutlen = 500
vim.opt.clipboard = "unnamedplus"
vim.opt.scrolloff = 3
vim.opt.lazyredraw = true
vim.opt.linebreak = true
vim.opt.number = false
vim.opt.numberwidth = 2
vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.smartindent = true
vim.opt.shortmess:append "casI"
vim.opt.whichwrap:append "<>hl"
vim.opt.guifont = "Liga SFMono Nerd Font:h14"

local disabled_built_ins = {
   "2html_plugin",
   "getscript",
   "getscriptPlugin",
   "gzip",
   "logipat",
   "netrw",
   "netrwPlugin",
   "netrwSettings",
   "netrwFileHandlers",
   "matchit",
   "tar",
   "tarPlugin",
   "rrhelper",
   "spellfile_plugin",
   "vimball",
   "vimballPlugin",
   "zip",
   "zipPlugin",
}

for _, plugin in pairs(disabled_built_ins) do
   vim.g["loaded_" .. plugin] = 1
end

