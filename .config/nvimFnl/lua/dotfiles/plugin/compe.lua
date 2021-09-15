local _2afile_2a = ".config/nvim/fnl/dotfiles/plugin/compe.fnl"
local _2amodule_name_2a = "dotfiles.plugin.compe"
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
nvim.o.completeopt = "menuone,noselect"
do
  local compe = require("compe")
  if compe then
    compe.setup({enabled = true, autocomplete = true, debug = false, min_length = 1, preselect = "enable", throttle_time = 80, source_timeout = 200, incomplete_delay = 400, max_abbr_width = 100, max_kind_width = 100, max_menu_width = 100, documentation = true, source = {path = true, buffer = true, calc = true, nvim_lsp = true, nvim_lua = true, conjure = true, vsnip = true}})
  else
  end
end
nvim.ex.inoremap("<silent><expr> <C-Space> compe#complete()")
nvim.ex.inoremap("<silent><expr> <CR> compe#confirm('<CR>')")
nvim.ex.inoremap("<silent><expr> <C-e> compe#close('<C-e>')")
nvim.ex.inoremap("<silent><expr> <C-f> compe#scroll({ 'delta': +4 })")
return nvim.ex.inoremap("<silent><expr> <C-d> compe#scroll({ 'delta': -4 })")