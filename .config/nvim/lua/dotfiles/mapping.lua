local _2afile_2a = "mapping.fnl"
local _2amodule_name_2a = "dotfiles.mapping"
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
local core, nu, nvim = autoload("aniseed.core"), autoload("aniseed.nvim.util"), autoload("aniseed.nvim")
do end (_2amodule_locals_2a)["core"] = core
_2amodule_locals_2a["nu"] = nu
_2amodule_locals_2a["nvim"] = nvim
local function noremap(mode, from, to)
  return nvim.set_keymap(mode, from, to, {noremap = true})
end
_2amodule_locals_2a["noremap"] = noremap
nvim.set_keymap("n", "<space>", "<nop>", {noremap = true})
nvim.g.mapleader = " "
nvim.g.maplocalleader = ","
noremap("i", "jk", "<esc>")
noremap("c", "jk", "<c-c>")
noremap("t", "jk", "<c-\\><c-n>")
noremap("n", "<leader>wm", ":tab sp<cr>")
noremap("n", "<leader>wc", ":only<cr>")
noremap("n", "<leader>bd", ":bdelete!<cr>")
noremap("n", "<leader>to", ":tabonly<cr>")
noremap("n", "<leader>sw", ":mksession! .quicksave.vim<cr>")
noremap("n", "<leader>sr", ":source .quicksave.vim<cr>")
noremap("n", "<leader>bo", ":call DeleteHiddenBuffers()<cr>")
noremap("n", "<leader>zz", ":normal! 1z=<cr>")
noremap("n", "<leader>bt", ":%s/\\s\\+$//e<cr>")
nu["fn-bridge"]("DeleteHiddenBuffers", "dotfiles.mapping", "delete-hidden-buffers")
local function delete_hidden_buffers()
  local visible_bufs = core.concat(unpack(core.map(nvim.fn.tabpagebuflist, nvim.fn.range(1, nvim.fn.tabpagenr("$")))))
  local function _1_(bufnr)
    return nvim.ex.bwipeout(bufnr)
  end
  local function _2_(bufnr)
    return (nvim.fn.bufexists(bufnr) and (-1 == nvim.fn.index(visible_bufs, bufnr)))
  end
  return core["run!"](_1_, core.filter(_2_, nvim.fn.range(1, nvim.fn.bufnr("$"))))
end
_2amodule_2a["delete-hidden-buffers"] = delete_hidden_buffers