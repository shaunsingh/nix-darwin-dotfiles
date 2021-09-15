local _2afile_2a = "plugin/colorizer.fnl"
local _2amodule_name_2a = "dotfiles.plugin.colorizer"
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
local colorizer = require("colorizer")
if colorizer then
  return colorizer.setup()
else
  return nil
end