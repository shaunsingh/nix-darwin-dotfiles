local utils = require "core.utils"

local map = utils.map
local maps = utils.mappings
local plugin_maps = maps.plugin

local cmd = vim.cmd

vim.g.mapleader = " "

local M = {}

-- these mappings will only be called during initialization
M.misc = function()
   local function non_config_mappings()
      -- Don't copy the replaced text after pasting in visual mode
      map("v", "p", '"_dP')

      -- Allow moving the cursor through wrapped lines with j, k, <Up> and <Down>
      -- http://www.reddit.com/r/vim/comments/2k4cbr/problem_with_gj_and_gk/
      -- empty mode is same as using :map
      map("", "j", 'v:count ? "j" : "gj"', { expr = true })
      map("", "k", 'v:count ? "k" : "gk"', { expr = true })
      map("", "<Down>", 'v:count ? "j" : "gj"', { expr = true })
      map("", "<Up>", 'v:count ? "k" : "gk"', { expr = true })

      -- use ESC to turn off search highlighting
      map("n", "<Esc>", ":noh <CR>")
   end

   local function required_mappings()
      local term_maps = maps.terminal
      -- get out of terminal mode
      map("t", term_maps.esc_termmode, "<C-\\><C-n>")
      -- hide a term from within terminal mode
      map("t", term_maps.esc_hide_termmode, "<C-\\><C-n> :lua require('core.utils').close_buffer() <CR>")
      -- pick a hidden term
      map("n", term_maps.pick_term, ":Telescope terms <CR>")
      map("n", term_maps.new_horizontal, ":execute 15 .. 'new +terminal' | let b:term_type = 'hori' | startinsert <CR>")
      map("n", term_maps.new_vertical, ":execute 'vnew +terminal' | let b:term_type = 'vert' | startinsert <CR>")
      map("n", term_maps.new_window, ":execute 'terminal' | let b:term_type = 'wind' | startinsert <CR>")
      -- terminal mappings end --

      -- Add Packer commands because we are not loading it at startup
      cmd "silent! command PackerClean lua require 'plugins' require('packer').clean()"
      cmd "silent! command PackerCompile lua require 'plugins' require('packer').compile()"
      cmd "silent! command PackerInstall lua require 'plugins' require('packer').install()"
      cmd "silent! command PackerStatus lua require 'plugins' require('packer').status()"
      cmd "silent! command PackerSync lua require 'plugins' require('packer').sync()"
      cmd "silent! command PackerUpdate lua require 'plugins' require('packer').update()"
   end

   non_config_mappings()
   required_mappings()
end

-- below are all plugin related mappinsg

M.better_escape = function()
   vim.g.better_escape_shortcut = plugin_maps.better_escape.esc_insertmode or { "" }
end

M.bufferline = function()
   local m = plugin_maps.bufferline

   map("n", m.next_buffer, ":BufferLineCycleNext <CR>")
   map("n", m.prev_buffer, ":BufferLineCyclePrev <CR>")
end

M.mapsheet = function()
   local m = plugin_maps.mapsheet

   map("n", m.default_keys, ":lua require('cheatsheet').show_cheatsheet_telescope() <CR>")
end

M.comment = function()
   local m = plugin_maps.comment.toggle
   map("n", m, ":CommentToggle <CR>")
   map("v", m, ":CommentToggle <CR>")
end

M.nvimtree = function()
   map("n", plugin_maps.nvimtree.toggle, ":NvimTreeToggle <CR>")
end

M.neoformat = function()
   map("n", plugin_maps.neoformat.format, ":Neoformat <CR>")
end

M.telescope = function()
   local m = plugin_maps.telescope

   map("n", m.buffers, ":Telescope buffers <CR>")
   map("n", m.find_files, ":Telescope find_files <CR>")
   map("n", m.git_commits, ":Telescope git_commits <CR>")
   map("n", m.git_status, ":Telescope git_status <CR>")
   map("n", m.help_tags, ":Telescope help_tags <CR>")
   map("n", m.live_grep, ":Telescope live_grep <CR>")
   map("n", m.oldfiles, ":Telescope oldfiles <CR>")
end

M.telescope_media = function()
   local m = plugin_maps.telescope_media

   map("n", m.media_files, ":Telescope media_files <CR>")
end

M.truezen = function()
   local m = plugin_maps.truezen

   map("n", m.ataraxis_mode, ":TZAtaraxis <CR>")
   map("n", m.focus_mode, ":TZFocus <CR>")
   map("n", m.minimalistic_mode, ":TZMinimalist <CR>")
end

return M
