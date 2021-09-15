local _2afile_2a = "plugin/telescope.fnl"
local _2amodule_name_2a = "dotfiles.plugin.telescope"
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
local nvim, telescope, util = autoload("aniseed.nvim"), autoload("telescope"), autoload("dotfiles.util")
do end (_2amodule_locals_2a)["nvim"] = nvim
_2amodule_locals_2a["telescope"] = telescope
_2amodule_locals_2a["util"] = util
telescope.setup({defaults = {vimgrep_arguments = {"rg", "--color=never", "--no-heading", "--with-filename", "--line-number", "--column", "--smart-case", "--hidden", "--follow", "-g", "!.git/"}}})
util.lnnoremap(".", "Telescope find_files hidden=true")
util.lnnoremap("f-", "Telescope file_browser")
util.lnnoremap("fg", "Telescope live_grep")
util.lnnoremap("*", "Telescope grep_string")
util.lnnoremap("fb", "Telescope buffers")
util.lnnoremap("fH", "Telescope help_tags")
util.lnnoremap("fm", "Telescope keymaps")
util.lnnoremap("fM", "Telescope marks")
util.lnnoremap("fr", "Telescope oldfiles")
util.lnnoremap("ft", "Telescope filetypes")
util.lnnoremap("fc", "Telescope commands")
util.lnnoremap("fC", "Telescope command_history")
util.lnnoremap("fq", "Telescope quickfix")
return util.lnnoremap("fl", "Telescope loclist")