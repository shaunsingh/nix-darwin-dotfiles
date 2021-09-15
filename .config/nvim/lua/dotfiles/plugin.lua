local _2afile_2a = "plugin.fnl"
local _2amodule_name_2a = "dotfiles.plugin"
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
local a, nvim, packer, util = autoload("aniseed.core"), autoload("aniseed.nvim"), autoload("packer"), autoload("dotfiles.util")
do end (_2amodule_locals_2a)["a"] = a
_2amodule_locals_2a["nvim"] = nvim
_2amodule_locals_2a["packer"] = packer
_2amodule_locals_2a["util"] = util
local function safe_require_plugin_config(name)
  local ok_3f, val_or_err = pcall(require, ("dotfiles.plugin." .. name))
  if not ok_3f then
    return print(("dotfiles error: " .. val_or_err))
  else
    return nil
  end
end
_2amodule_2a["safe-require-plugin-config"] = safe_require_plugin_config
local function use(...)
  local pkgs = {...}
  local function _2_(use0)
    for i = 1, a.count(pkgs), 2 do
      local name = pkgs[i]
      local opts = pkgs[(i + 1)]
      do
        local _3_ = opts.mod
        if _3_ then
          safe_require_plugin_config(_3_)
        else
        end
      end
      use0(a.assoc(opts, 1, name))
    end
    return nil
  end
  return packer.startup(_2_)
end
_2amodule_locals_2a["use"] = use
return use("wbthomason/packer.nvim", {}, "Olical/aniseed", {branch = "develop"}, "Olical/conjure", {branch = "develop", mod = "conjure"}, "Olical/nvim-local-fennel", {}, "shaunsingh/nord.nvim", {mod = "nord"}, "nvim-telescope/telescope.nvim", {requires = {{"nvim-lua/popup.nvim"}, {"nvim-lua/plenary.nvim"}}, mod = "telescope"}, "nvim-treesitter/nvim-treesitter", {run = ":TSUpdate", mod = "treesitter"}, "ggandor/lightspeed.nvim", {})