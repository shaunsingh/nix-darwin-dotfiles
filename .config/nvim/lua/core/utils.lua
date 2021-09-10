local M = {}

M.close_buffer = function(bufexpr, force)
   -- Options
   local opts = {
      next = "cycle", -- how to retrieve the next buffer
      quit = false, -- exit when last buffer is deleted
   }

   -- Switch to buffer 'buf' on each window from list 'windows'
   local function switch_buffer(windows, buf)
      local cur_win = vim.fn.winnr()
      for _, winid in ipairs(windows) do
         vim.cmd(string.format("%d wincmd w", vim.fn.win_id2win(winid)))
         vim.cmd(string.format("buffer %d", buf))
      end
      vim.cmd(string.format("%d wincmd w", cur_win)) -- return to original window
   end

   -- Select the first buffer with a number greater than given buffer
   local function get_next_buf(buf)
      local next = vim.fn.bufnr "#"
      if opts.next == "alternate" and vim.fn.buflisted(next) == 1 then
         return next
      end
      for i = 0, vim.fn.bufnr "$" - 1 do
         next = (buf + i) % vim.fn.bufnr "$" + 1 -- will loop back to 1
         if vim.fn.buflisted(next) == 1 then
            return next
         end
      end
   end

   local buf = vim.fn.bufnr()
   if vim.fn.buflisted(buf) == 0 then -- exit if buffer number is invalid
      vim.cmd "close"
      return
   end

   if #vim.fn.getbufinfo { buflisted = 1 } < 2 then
      if opts.quit then
         -- exit when there is only one buffer left
         if force then
            vim.cmd "qall!"
         else
            vim.cmd "confirm qall"
         end
         return
      end

      local doom_term, type = pcall(function()
         return vim.api.nvim_buf_get_var(buf, "term_type")
      end)

      if doom_term then
         -- Must be a window type
         vim.cmd(string.format("setlocal nobl", buf))
         vim.cmd "enew"
         return
      end
      -- don't exit and create a new empty buffer
      vim.cmd "enew"
      vim.cmd "bp"
   end

   local next_buf = get_next_buf(buf)
   local windows = vim.fn.getbufinfo(buf)[1].windows

   -- force deletion of terminal buffers to avoid the prompt
   if force or vim.fn.getbufvar(buf, "&buftype") == "terminal" then
      local doom_term, type = pcall(function()
         return vim.api.nvim_buf_get_var(buf, "term_type")
      end)

      -- TODO this scope is error prone, make resilient
      if doom_term then
         if type == "wind" then
            -- hide from bufferline
            vim.cmd(string.format("%d bufdo setlocal nobl", buf))
            -- swtich to another buff
            -- TODO switch to next bufffer, this works too
            vim.cmd "BufferLineCycleNext"
         else
            local cur_win = vim.fn.winnr()
            -- we can close this window
            vim.cmd(string.format("%d wincmd c", cur_win))
            return
         end
      else
         switch_buffer(windows, next_buf)
         vim.cmd(string.format("bd! %d", buf))
      end
   else
      switch_buffer(windows, next_buf)
      vim.cmd(string.format("silent! confirm bd %d", buf))
   end
   -- revert buffer switches if user has canceled deletion
   if vim.fn.buflisted(buf) == 1 then
      switch_buffer(windows, buf)
   end
end

M.map = function(mode, keys, cmd, opt)
   local options = { noremap = true, silent = true }
   if opt then
      options = vim.tbl_extend("force", options, opt)
   end

   -- all valid modes allowed for mappings
   -- :h map-modes
   local valid_modes = {
      [""] = true,
      ["n"] = true,
      ["v"] = true,
      ["s"] = true,
      ["x"] = true,
      ["o"] = true,
      ["!"] = true,
      ["i"] = true,
      ["l"] = true,
      ["c"] = true,
      ["t"] = true,
   }

   -- helper function for M.map
   -- can gives multiple modes and keys
   local function map_wrapper(mode, lhs, rhs, options)
      if type(lhs) == "table" then
         for _, key in ipairs(lhs) do
            map_wrapper(mode, key, rhs, options)
         end
      else
         if type(mode) == "table" then
            for _, m in ipairs(mode) do
               map_wrapper(m, lhs, rhs, options)
            end
         else
            if valid_modes[mode] and lhs and rhs then
               vim.api.nvim_set_keymap(mode, lhs, rhs, options)
            else
               mode, lhs, rhs = mode or "", lhs or "", rhs or ""
               print("Cannot set mapping [ mode = '" .. mode .. "' | key = '" .. lhs .. "' | cmd = '" .. rhs .. "' ]")
            end
         end
      end
   end

   map_wrapper(mode, keys, cmd, options)
end

M.merge_table = function(into, from, nodes_to_replace)
   -- make sure both are table
   if type(into) ~= "table" or type(from) ~= "table" then
      return into
   end

   local stack, seen = {}, {}
   local table1, table2 = into, from

   if type(nodes_to_replace) == "table" then
      -- function that will be executed with loadstring
      local base_fn = [[
return function(table1, table2)
   local t1, t2 = table1_node or false , table2_node or false
   if t1 and t2 then
      table1_node = table2_node
   end
   return table1
end]]
      for _, node in ipairs(nodes_to_replace) do
         local fn = base_fn:gsub("_node", node)
         table1 = loadstring(fn)()(table1, table2)
      end
   end

   while true do
      for k, v in pairs(table2) do
         if type(v) == "table" and type(table1[k]) == "table" then
            table.insert(stack, { table1[k], table2[k] })
         else
            local present = seen[v] or false
            if not present then
               if type(k) == "number" then
                  for _, value in pairs(table1) do
                     if value == v then
                        present = true
                        break
                     end
                  end
                  seen[v] = true
                  if not present then
                     table1[#table1 + 1] = v
                  end
               else
                  table1[k] = v
               end
            end
         end
      end
      if #stack > 0 then
         local t = stack[#stack]
         table1, table2 = t[1], t[2]
         stack[#stack] = nil
      else
         break
      end
   end
   return into
end

-- load plugin after entering vim ui
M.packer_lazy_load = function(plugin, timer)
   if plugin then
      timer = timer or 0
      vim.defer_fn(function()
         require("packer").loader(plugin)
      end, timer)
   end
end

M.mappings = {
   -- close current focused buffer
   close_buffer = "<leader>x",
   copy_whole_file = "<C-a>", -- copy all contents of the current buffer

   -- navigation in insert mode, only if enabled in options
   insert_nav = {
      backward = "<C-h>",
      end_of_line = "<C-e>",
      forward = "<C-l>",
      next_line = "<C-k>",
      prev_line = "<C-j>",
      top_of_line = "<C-a>",
   },

   line_number_toggle = "<leader>n", -- show or hide line number
   new_buffer = "<S-t>", -- open a new buffer
   new_tab = "<C-t>b", -- open a new vim tab
   save_file = "<C-s>", -- save file using :w

   -- terminal related mappings
   terminal = {
      esc_termmode = { "jk" }, -- multiple mappings allowed
      esc_hide_termmode = { "JK" }, -- multiple mappings allowed
      pick_term = "<leader>W",
      new_horizontal = "<leader>h",
      new_vertical = "<leader>v",
      new_window = "<leader>w",
   },
}

-- all plugins related mappings
M.mappings.plugin = {
   bufferline = {
      next_buffer = "<TAB>", -- next buffer
      prev_buffer = "<S-Tab>", -- previous buffer
      --better window movement
      moveLeft = "<C-h>",
      moveRight = "<C-l>",
      moveUp = "<C-k>",
      moveDown = "<C-j>",
   },
   mapsheet = {
      default_keys = "<leader>?",
   },
   comment = {
      toggle = "<leader>/", -- trigger comment on a single/selected lines/number prefix
   },
   better_escape = {
      esc_insertmode = { "jk" }, -- multiple mappings allowed
   },
   nvimtree = {
      toggle = "<leader>op", -- file manager
   },
   neoformat = {
      format = "<leader>cf",
   },
   telescope = {
      buffers = "<leader>bb",
      find_files = "<leader>.",
      git_commits = "<leader>cm",
      git_status = "<leader>gt",
      help_tags = "<leader>fh",
      live_grep = "<leader>sp",
      oldfiles = "<leader>fr",
   },
   telescope_media = {
      media_files = "<leader>fp",
   },
   truezen = { -- distraction free modes mapping, hide statusline, tabline, line numbers
      ataraxis_mode = "<leader>tz", -- center
      focus_mode = "<leader>zf",
      minimalistic_mode = "<leader>zm", -- as it is
   },
}

return M
