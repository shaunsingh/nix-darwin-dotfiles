local _2afile_2a = ".config/nvimFnl/fnl/dotfiles/plugin/nord.fnl"
local _2amodule_name_2a = "dotfiles.plugin.nord"
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
local nord, nvim = autoload("nord"), autoload("aniseed.nvim")
do end (_2amodule_locals_2a)["nord"] = nord
_2amodule_locals_2a["nvim"] = nvim
return nvim.ex.colorscheme("nord")