-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  LuaSnip = {
    after = { "cmp_luasnip" },
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0û\0\0\0†\0\0\0\0\0\0\2\6\0\0\0\5\0\0\0A@\0\0\28Ä\0\1\6Ä@\0\28@Ä\0\30\0Ä\0\3\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\15\0\0\0\0\0\0\0plugins.others\0\4\b\0\0\0\0\0\0\0luasnip\0\0\0\0\0\6\0\0\0ü\0\0\0ü\0\0\0ü\0\0\0ü\0\0\0ü\0\0\0†\0\0\0\0\0\0\0\0\0\0\0" },
    load_after = {
      ["nvim-cmp"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/LuaSnip",
    wants = { "friendly-snippets" }
  },
  ["TrueZen.nvim"] = {
    commands = { "TZAtaraxis", "TZMinimalist", "TZFocus" },
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0⁄\0\0\0‹\0\0\0\0\0\0\2\4\0\0\0\5\0\0\0A@\0\0\28@\0\1\30\0Ä\0\2\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\16\0\0\0\0\0\0\0plugins.zenmode\0\0\0\0\0\4\0\0\0€\0\0\0€\0\0\0€\0\0\0‹\0\0\0\0\0\0\0\0\0\0\0" },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/TrueZen.nvim"
  },
  ["alpha-nvim"] = {
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0005\0\0\0007\0\0\0\0\0\0\2\4\0\0\0\5\0\0\0A@\0\0\28@\0\1\30\0Ä\0\2\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\14\0\0\0\0\0\0\0plugins.alpha\0\0\0\0\0\4\0\0\0006\0\0\0006\0\0\0006\0\0\0007\0\0\0\0\0\0\0\0\0\0\0" },
    load_after = {
      ["nord.nvim"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/alpha-nvim"
  },
  ["better-escape.nvim"] = {
    config = { '\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0\27\0\0\0"\0\0\0\0\0\0\5\18\0\0\0\5\0\0\0A@\0\0\28Ä\0\1\6Ä@\0J\0\1\0ä\0\0\1¡\0\1\0\1A\1\0¢@\0\1IÄÄÅÖ¿\1\0Ü\0B\1Ü@B\1IÄ\0ÉI¿BÖI@CÜ\28@\0\1\30\0Ä\0\14\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\14\0\0\0\0\0\0\0better_escape\0\4\6\0\0\0\0\0\0\0setup\0\4\b\0\0\0\0\0\0\0mapping\0\4\3\0\0\0\0\0\0\0jk\0\4\3\0\0\0\0\0\0\0jj\0\4\b\0\0\0\0\0\0\0timeout\0\4\4\0\0\0\0\0\0\0vim\0\4\2\0\0\0\0\0\0\0o\0\4\v\0\0\0\0\0\0\0timeoutlen\0\4\18\0\0\0\0\0\0\0clear_empty_lines\0\1\1\4\5\0\0\0\0\0\0\0keys\0\4\6\0\0\0\0\0\0\0<Esc>\0\0\0\0\0\18\0\0\0\28\0\0\0\28\0\0\0\28\0\0\0\28\0\0\0\28\0\0\0\29\0\0\0\29\0\0\0\29\0\0\0\29\0\0\0\29\0\0\0\30\0\0\0\30\0\0\0\30\0\0\0\30\0\0\0\31\0\0\0 \0\0\0\28\0\0\0"\0\0\0\0\0\0\0\0\0\0\0' },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/better-escape.nvim"
  },
  ["bufferline.nvim"] = {
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0J\0\0\0L\0\0\0\0\0\0\2\4\0\0\0\5\0\0\0A@\0\0\28@\0\1\30\0Ä\0\2\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\19\0\0\0\0\0\0\0plugins.bufferline\0\0\0\0\0\4\0\0\0K\0\0\0K\0\0\0K\0\0\0L\0\0\0\0\0\0\0\0\0\0\0" },
    load_after = {
      ["nvim-web-devicons"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/bufferline.nvim"
  },
  ["cmp-buffer"] = {
    after_files = { "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp-buffer/after/plugin/cmp_buffer.lua" },
    load_after = {
      ["cmp-nvim-lsp"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp-buffer"
  },
  ["cmp-nvim-lsp"] = {
    after = { "cmp-buffer" },
    after_files = { "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lsp/after/plugin/cmp_nvim_lsp.lua" },
    load_after = {},
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lsp"
  },
  ["cmp-nvim-lua"] = {
    after_files = { "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lua/after/plugin/cmp_nvim_lua.lua" },
    load_after = {
      cmp_luasnip = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lua"
  },
  cmp_luasnip = {
    after = { "cmp-nvim-lua" },
    after_files = { "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp_luasnip/after/plugin/cmp_luasnip.lua" },
    load_after = {
      LuaSnip = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp_luasnip"
  },
  ["diffview.nvim"] = {
    loaded = true,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/start/diffview.nvim"
  },
  ["friendly-snippets"] = {
    after = { "nvim-cmp" },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/friendly-snippets"
  },
  ["galaxyline.nvim"] = {
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0B\0\0\0D\0\0\0\0\0\0\2\4\0\0\0\5\0\0\0A@\0\0\28@\0\1\30\0Ä\0\2\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\19\0\0\0\0\0\0\0plugins.statusline\0\0\0\0\0\4\0\0\0C\0\0\0C\0\0\0C\0\0\0D\0\0\0\0\0\0\0\0\0\0\0" },
    load_after = {
      ["nvim-web-devicons"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/galaxyline.nvim"
  },
  ["gitsigns.nvim"] = {
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0n\0\0\0p\0\0\0\0\0\0\2\4\0\0\0\5\0\0\0A@\0\0\28@\0\1\30\0Ä\0\2\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\17\0\0\0\0\0\0\0plugins.gitsigns\0\0\0\0\0\4\0\0\0o\0\0\0o\0\0\0o\0\0\0p\0\0\0\0\0\0\0\0\0\0\0" },
    loaded = true,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/start/gitsigns.nvim"
  },
  hop = {
    commands = { "HopWord", "HopLine", "HopChar1", "HopChar2", "HopPattern" },
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0Ù\0\0\0ˆ\0\0\0\0\0\0\2\6\0\0\0\5\0\0\0A@\0\0\28Ä\0\1\6Ä@\0\28@Ä\0\30\0Ä\0\3\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\4\0\0\0\0\0\0\0hop\0\4\6\0\0\0\0\0\0\0setup\0\0\0\0\0\6\0\0\0ı\0\0\0ı\0\0\0ı\0\0\0ı\0\0\0ı\0\0\0ˆ\0\0\0\0\0\0\0\0\0\0\0" },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/hop"
  },
  ["indent-blankline.nvim"] = {
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0R\0\0\0T\0\0\0\0\0\0\2\6\0\0\0\5\0\0\0A@\0\0\28Ä\0\1\6Ä@\0\28@Ä\0\30\0Ä\0\3\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\15\0\0\0\0\0\0\0plugins.others\0\4\n\0\0\0\0\0\0\0blankline\0\0\0\0\0\6\0\0\0S\0\0\0S\0\0\0S\0\0\0S\0\0\0S\0\0\0T\0\0\0\0\0\0\0\0\0\0\0" },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/indent-blankline.nvim"
  },
  ["lsp_signature.nvim"] = {
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0á\0\0\0â\0\0\0\0\0\0\2\6\0\0\0\5\0\0\0A@\0\0\28Ä\0\1\6Ä@\0\28@Ä\0\30\0Ä\0\3\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\15\0\0\0\0\0\0\0plugins.others\0\4\n\0\0\0\0\0\0\0signature\0\0\0\0\0\6\0\0\0à\0\0\0à\0\0\0à\0\0\0à\0\0\0à\0\0\0â\0\0\0\0\0\0\0\0\0\0\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/lsp_signature.nvim"
  },
  neogit = {
    commands = { "Neogit", "Neogit commit" },
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0\v\1\0\0\r\1\0\0\0\0\0\2\4\0\0\0\5\0\0\0A@\0\0\28@\0\1\30\0Ä\0\2\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\15\0\0\0\0\0\0\0plugins.neogit\0\0\0\0\0\4\0\0\0\f\1\0\0\f\1\0\0\f\1\0\0\r\1\0\0\0\0\0\0\0\0\0\0" },
    loaded = false,
    needs_bufread = true,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/neogit"
  },
  ["nord.nvim"] = {
    after = { "alpha-nvim", "nvim-web-devicons" },
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0(\0\0\0.\0\0\0\0\0\0\2\17\0\0\0\5\0\0\0\6@@\0\t¿@Å\5\0\0\0\6@@\0\t¿@Ç\5\0\0\0\6@@\0\tÄ¡Ç\5\0\0\0\6@@\0\t¿¿É\5\0\0\0\6\0B\0A@\2\0\28@\0\1\30\0Ä\0\n\0\0\0\4\4\0\0\0\0\0\0\0vim\0\4\2\0\0\0\0\0\0\0g\0\4\r\0\0\0\0\0\0\0nord_borders\0\1\0\4\14\0\0\0\0\0\0\0nord_contrast\0\4\28\0\0\0\0\0\0\0nord_cursorline_transparent\0\1\1\4\24\0\0\0\0\0\0\0nord_disable_background\0\4\4\0\0\0\0\0\0\0cmd\0\4\17\0\0\0\0\0\0\0colorscheme nord\0\0\0\0\0\17\0\0\0)\0\0\0)\0\0\0)\0\0\0*\0\0\0*\0\0\0*\0\0\0+\0\0\0+\0\0\0+\0\0\0,\0\0\0,\0\0\0,\0\0\0-\0\0\0-\0\0\0-\0\0\0-\0\0\0.\0\0\0\0\0\0\0\0\0\0\0" },
    load_after = {
      ["packer.nvim"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/nord.nvim"
  },
  ["nvim-autopairs"] = {
    load_after = {
      ["nvim-cmp"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-autopairs"
  },
  ["nvim-cmp"] = {
    after = { "LuaSnip", "nvim-autopairs" },
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0ï\0\0\0ó\0\0\0\0\0\0\2\4\0\0\0\5\0\0\0A@\0\0\28@\0\1\30\0Ä\0\2\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\f\0\0\0\0\0\0\0plugins.cmp\0\0\0\0\0\4\0\0\0ñ\0\0\0ñ\0\0\0ñ\0\0\0ó\0\0\0\0\0\0\0\0\0\0\0" },
    load_after = {
      ["friendly-snippets"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-cmp"
  },
  ["nvim-code-action-menu"] = {
    commands = { "CodeActionMenu" },
    loaded = false,
    needs_bufread = true,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-code-action-menu"
  },
  ["nvim-colorizer.lua"] = {
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0Z\0\0\0\\\0\0\0\0\0\0\2\6\0\0\0\5\0\0\0A@\0\0\28Ä\0\1\6Ä@\0\28@Ä\0\30\0Ä\0\3\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\15\0\0\0\0\0\0\0plugins.others\0\4\n\0\0\0\0\0\0\0colorizer\0\0\0\0\0\6\0\0\0[\0\0\0[\0\0\0[\0\0\0[\0\0\0[\0\0\0\\\0\0\0\0\0\0\0\0\0\0\0" },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-colorizer.lua"
  },
  ["nvim-lspconfig"] = {
    after = { "cmp-nvim-lsp", "lsp_signature.nvim" },
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0v\0\0\0x\0\0\0\0\0\0\2\4\0\0\0\5\0\0\0A@\0\0\28@\0\1\30\0Ä\0\2\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\18\0\0\0\0\0\0\0plugins.lspconfig\0\0\0\0\0\4\0\0\0w\0\0\0w\0\0\0w\0\0\0x\0\0\0\0\0\0\0\0\0\0\0" },
    loaded = true,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-lspconfig"
  },
  ["nvim-lspinstall"] = {
    loaded = true,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/start/nvim-lspinstall"
  },
  ["nvim-tree.lua"] = {
    commands = { "NvimTreeToggle", "NvimTreeFocus" },
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0¿\0\0\0¬\0\0\0\0\0\0\2\4\0\0\0\5\0\0\0A@\0\0\28@\0\1\30\0Ä\0\2\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\17\0\0\0\0\0\0\0plugins.nvimtree\0\0\0\0\0\4\0\0\0¡\0\0\0¡\0\0\0¡\0\0\0¬\0\0\0\0\0\0\0\0\0\0\0" },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    after = { "nvim-ts-rainbow" },
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0b\0\0\0d\0\0\0\0\0\0\2\4\0\0\0\5\0\0\0A@\0\0\28@\0\1\30\0Ä\0\2\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\19\0\0\0\0\0\0\0plugins.treesitter\0\0\0\0\0\4\0\0\0c\0\0\0c\0\0\0c\0\0\0d\0\0\0\0\0\0\0\0\0\0\0" },
    loaded = false,
    needs_bufread = true,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-treesitter"
  },
  ["nvim-ts-rainbow"] = {
    load_after = {
      ["nvim-treesitter"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-ts-rainbow"
  },
  ["nvim-web-devicons"] = {
    after = { "galaxyline.nvim", "bufferline.nvim" },
    load_after = {
      ["nord.nvim"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-web-devicons"
  },
  ["orgmode.nvim"] = {
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0¸\0\0\0˛\0\0\0\0\0\0\2\6\0\0\0\5\0\0\0A@\0\0\28Ä\0\1\6Ä@\0\28@Ä\0\30\0Ä\0\3\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\15\0\0\0\0\0\0\0plugins.others\0\4\b\0\0\0\0\0\0\0orgmode\0\0\0\0\0\6\0\0\0˝\0\0\0˝\0\0\0˝\0\0\0˝\0\0\0˝\0\0\0˛\0\0\0\0\0\0\0\0\0\0\0" },
    loaded = false,
    needs_bufread = true,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim"
  },
  ["packer.nvim"] = {
    after = { "nord.nvim" },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["telescope-fzf-native.nvim"] = {
    loaded = true,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/start/telescope-fzf-native.nvim"
  },
  ["telescope.nvim"] = {
    commands = { "Telescope" },
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0Œ\0\0\0–\0\0\0\0\0\0\2\4\0\0\0\5\0\0\0A@\0\0\28@\0\1\30\0Ä\0\2\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\18\0\0\0\0\0\0\0plugins.telescope\0\0\0\0\0\4\0\0\0œ\0\0\0œ\0\0\0œ\0\0\0–\0\0\0\0\0\0\0\0\0\0\0" },
    loaded = false,
    needs_bufread = true,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/telescope.nvim"
  },
  ["twilight.nvim"] = {
    commands = { "Twilight", "TwilightEnable" },
    config = { "\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0Â\0\0\0Á\0\0\0\0\0\0\2\a\0\0\0\5\0\0\0A@\0\0\28Ä\0\1\6Ä@\0J\0\0\0\28@\0\1\30\0Ä\0\3\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\t\0\0\0\0\0\0\0twilight\0\4\6\0\0\0\0\0\0\0setup\0\0\0\0\0\a\0\0\0Ê\0\0\0Ê\0\0\0Ê\0\0\0Ê\0\0\0Ê\0\0\0Ê\0\0\0Á\0\0\0\0\0\0\0\0\0\0\0" },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/twilight.nvim"
  },
  ["vim-startuptime"] = {
    commands = { "StartupTime" },
    loaded = false,
    needs_bufread = false,
    path = "/Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/vim-startuptime"
  }
}

time([[Defining packer_plugins]], false)
local module_lazy_loads = {
  ["^cmp"] = "nvim-cmp",
  ["^cmp_nvim_lsp"] = "cmp-nvim-lsp"
}
local lazy_load_called = {['packer.load'] = true}
local function lazy_load_module(module_name)
  local to_load = {}
  if lazy_load_called[module_name] then return nil end
  lazy_load_called[module_name] = true
  for module_pat, plugin_name in pairs(module_lazy_loads) do
    if not _G.packer_plugins[plugin_name].loaded and string.match(module_name, module_pat) then
      to_load[#to_load + 1] = plugin_name
    end
  end

  if #to_load > 0 then
    require('packer.load')(to_load, {module = module_name}, _G.packer_plugins)
    local loaded_mod = package.loaded[module_name]
    if loaded_mod then
      return function(modname) return loaded_mod end
    end
  end
end

if not vim.g.packer_custom_loader_enabled then
  table.insert(package.loaders, 1, lazy_load_module)
  vim.g.packer_custom_loader_enabled = true
end

-- Config for: gitsigns.nvim
time([[Config for gitsigns.nvim]], true)
try_loadstring("\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0n\0\0\0p\0\0\0\0\0\0\2\4\0\0\0\5\0\0\0A@\0\0\28@\0\1\30\0Ä\0\2\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\17\0\0\0\0\0\0\0plugins.gitsigns\0\0\0\0\0\4\0\0\0o\0\0\0o\0\0\0o\0\0\0p\0\0\0\0\0\0\0\0\0\0\0", "config", "gitsigns.nvim")
time([[Config for gitsigns.nvim]], false)
-- Load plugins in order defined by `after`
time([[Sequenced loading]], true)
vim.cmd [[ packadd nvim-lspconfig ]]

-- Config for: nvim-lspconfig
try_loadstring("\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0v\0\0\0x\0\0\0\0\0\0\2\4\0\0\0\5\0\0\0A@\0\0\28@\0\1\30\0Ä\0\2\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\18\0\0\0\0\0\0\0plugins.lspconfig\0\0\0\0\0\4\0\0\0w\0\0\0w\0\0\0w\0\0\0x\0\0\0\0\0\0\0\0\0\0\0", "config", "nvim-lspconfig")

vim.cmd [[ packadd lsp_signature.nvim ]]

-- Config for: lsp_signature.nvim
try_loadstring("\27LuaQ\0\1\4\b\4\b\0005\0\0\0\0\0\0\0@/Users/shauryasingh/.config/nvim/lua/pluginList.lua\0á\0\0\0â\0\0\0\0\0\0\2\6\0\0\0\5\0\0\0A@\0\0\28Ä\0\1\6Ä@\0\28@Ä\0\30\0Ä\0\3\0\0\0\4\b\0\0\0\0\0\0\0require\0\4\15\0\0\0\0\0\0\0plugins.others\0\4\n\0\0\0\0\0\0\0signature\0\0\0\0\0\6\0\0\0à\0\0\0à\0\0\0à\0\0\0à\0\0\0à\0\0\0â\0\0\0\0\0\0\0\0\0\0\0", "config", "lsp_signature.nvim")

time([[Sequenced loading]], false)

-- Command lazy-loads
time([[Defining lazy-load commands]], true)
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopChar1 lua require("packer.load")({'hop'}, { cmd = "HopChar1", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file NvimTreeFocus lua require("packer.load")({'nvim-tree.lua'}, { cmd = "NvimTreeFocus", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[au CmdUndefined Neogit commit ++once lua require"packer.load"({'neogit'}, {}, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopLine lua require("packer.load")({'hop'}, { cmd = "HopLine", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopPattern lua require("packer.load")({'hop'}, { cmd = "HopPattern", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TwilightEnable lua require("packer.load")({'twilight.nvim'}, { cmd = "TwilightEnable", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopWord lua require("packer.load")({'hop'}, { cmd = "HopWord", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file CodeActionMenu lua require("packer.load")({'nvim-code-action-menu'}, { cmd = "CodeActionMenu", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TZAtaraxis lua require("packer.load")({'TrueZen.nvim'}, { cmd = "TZAtaraxis", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Neogit lua require("packer.load")({'neogit'}, { cmd = "Neogit", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file StartupTime lua require("packer.load")({'vim-startuptime'}, { cmd = "StartupTime", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Twilight lua require("packer.load")({'twilight.nvim'}, { cmd = "Twilight", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Telescope lua require("packer.load")({'telescope.nvim'}, { cmd = "Telescope", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TZMinimalist lua require("packer.load")({'TrueZen.nvim'}, { cmd = "TZMinimalist", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TZFocus lua require("packer.load")({'TrueZen.nvim'}, { cmd = "TZFocus", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopChar2 lua require("packer.load")({'hop'}, { cmd = "HopChar2", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file NvimTreeToggle lua require("packer.load")({'nvim-tree.lua'}, { cmd = "NvimTreeToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
time([[Defining lazy-load commands]], false)

vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time([[Defining lazy-load filetype autocommands]], true)
vim.cmd [[au FileType org ++once lua require("packer.load")({'orgmode.nvim'}, { ft = "org" }, _G.packer_plugins)]]
time([[Defining lazy-load filetype autocommands]], false)
  -- Event lazy-loads
time([[Defining lazy-load event autocommands]], true)
vim.cmd [[au InsertEnter * ++once lua require("packer.load")({'better-escape.nvim', 'friendly-snippets'}, { event = "InsertEnter *" }, _G.packer_plugins)]]
vim.cmd [[au VimEnter * ++once lua require("packer.load")({'packer.nvim'}, { event = "VimEnter *" }, _G.packer_plugins)]]
vim.cmd [[au BufRead * ++once lua require("packer.load")({'indent-blankline.nvim', 'nvim-colorizer.lua', 'nvim-treesitter'}, { event = "BufRead *" }, _G.packer_plugins)]]
time([[Defining lazy-load event autocommands]], false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time([[Sourcing ftdetect script at: /Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org.vim]], true)
vim.cmd [[source /Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org.vim]]
time([[Sourcing ftdetect script at: /Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org.vim]], false)
time([[Sourcing ftdetect script at: /Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org_archive.vim]], true)
vim.cmd [[source /Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org_archive.vim]]
time([[Sourcing ftdetect script at: /Users/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org_archive.vim]], false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
