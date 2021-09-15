local _2afile_2a = "lualine.fnl"
local _2amodule_name_2a = "dotfiles.plugin.lualine"
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
local lualine = require("lualine")
return lualine.setup({options = {theme = "nord", icons_enabled = true}, sections = {lualine_a = {{"mode", {upper = true}}}, lualine_b = {{"branch", {icon = "\238\130\160"}}}, lualine_c = {{"filename", {file_status = true}}}, lualine_x = {"encoding", "fileformat", "filetype"}, lualine_y = {"progress"}, lualine_z = {"location"}}, extensions = {}})