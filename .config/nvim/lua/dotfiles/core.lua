local _2afile_2a = "core.fnl"
local _2amodule_name_2a = "dotfiles.core"
local _2amodule_2a
do
  package.loaded[_2amodule_name_2a] = {}
  _2amodule_2a = package.loaded[_2amodule_name_2a]
end
local _2amodule_locals_2a
do
  _2amodule_2a["aniseed/locals"] = {}
  _2amodule_locals_2a = (_2amodule_2a)["aniseed/locals"]
end
local autoload = (require("aniseed.autoload")).autoload
local nvim = autoload("aniseed.nvim")
do end (_2amodule_locals_2a)["nvim"] = nvim
nvim.o.termguicolors = true
nvim.o.undofile = false
nvim.o.ruler = true
nvim.o.hidden = true
nvim.o.expandtab = true
nvim.o.ignorecase = true
nvim.o.splitbelow = true
nvim.o.splitright = true
nvim.o.cul = true
nvim.o.lazyredraw = true
nvim.o.linebreak = true
nvim.o.number = false
nvim.o.numberwidth = 2
nvim.o.laststatus = 0
nvim.o.mouse = "a"
nvim.o.signcolumn = "yes"
nvim.o.clipboard = "unnamedplus"
nvim.o.updatetime = 500
nvim.o.timeoutlen = 500
nvim.o.cmdheight = 1
nvim.o.scrolloff = 3
nvim.o.expandtab = true
nvim.o.shiftwidth = 4
nvim.o.smartindent = true
nvim.ex.set("spell")
nvim.ex.set("list")
nvim.ex.set("nonumber")
nvim.ex.set("wildmode=full")
nvim.ex.set("wildoptions=pum")
nvim.ex.set("listchars-=eol:\226\134\181")
return nvim.ex.set("clipboard-=unnamedplus")