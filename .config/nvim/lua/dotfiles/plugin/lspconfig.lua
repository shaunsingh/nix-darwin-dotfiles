local _2afile_2a = "plugin/lspconfig.fnl"
local _2amodule_name_2a = "dotfiles.plugin.lspconfig"
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
local nvim, util = autoload("aniseed.nvim"), autoload("dotfiles.util")
do end (_2amodule_locals_2a)["nvim"] = nvim
_2amodule_locals_2a["util"] = util
local function map(from, to)
  return util.nnoremap(from, to)
end
_2amodule_locals_2a["map"] = map
local lsp = require("lspconfig")
if lsp then
  lsp.clojure_lsp.setup({})
  map("gd", "lua vim.lsp.buf.definition()")
  map("gD", "lua vim.lsp.buf.declaration()")
  map("gr", "lua vim.lsp.buf.references()")
  map("gi", "lua vim.lsp.buf.implementation()")
  map("K", "lua vim.lsp.buf.hover()")
  map("<c-k>", "lua vim.lsp.buf.signature_help()")
  map("<c-n>", "lua vim.lsp.diagnostic.goto_prev()")
  map("<c-p>", "lua vim.lsp.diagnostic.goto_next()")
  map("<leader>lr", "lua vim.lsp.buf.rename()")
  return map("<leader>lf", "lua vim.lsp.buf.formatting()")
else
  return nil
end