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
local package_path_str = "/home/shauryasingh/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/shauryasingh/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/shauryasingh/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/shauryasingh/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/shauryasingh/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

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
    config = { "\27LJ\2\n∆\1\0\0\5\0\b\0\0176\0\0\0006\2\1\0'\3\2\0B\0\3\3\14\0\0\0X\2\1ÄK\0\1\0009\2\3\0019\2\4\0025\4\5\0B\2\2\0016\2\1\0'\4\6\0B\2\2\0029\2\a\2B\2\1\1K\0\1\0\tload luasnip/loaders/from_vscode\1\0\2\17updateevents\29TextChanged,TextChangedI\fhistory\2\15set_config\vconfig\fluasnip\frequire\npcall\0" },
    load_after = {
      ["nvim-cmp"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/LuaSnip",
    wants = { "friendly-snippets" }
  },
  ["TrueZen.nvim"] = {
    commands = { "TZAtaraxis", "TZMinimalist", "TZFocus" },
    config = { "\27LJ\2\n≠\a\0\0\t\0\"\0/6\0\0\0006\2\1\0'\3\2\0B\0\3\3\14\0\0\0X\2\1ÄK\0\1\0009\2\3\0015\4\v\0005\5\5\0005\6\4\0=\6\6\0055\6\a\0=\6\b\0055\6\t\0=\6\n\5=\5\f\0045\5\25\0005\6\r\0005\a\14\0=\a\15\0065\a\16\0004\b\0\0=\b\17\a4\b\0\0=\b\18\a4\b\0\0=\b\19\a4\b\0\0=\b\20\a4\b\0\0=\b\21\a4\b\0\0=\b\22\a4\b\0\0=\b\23\a=\a\24\6=\6\26\0055\6\27\0=\6\28\5=\5\29\0045\5\30\0=\5\31\0045\5 \0=\5!\4B\2\2\1K\0\1\0\tmisc\1\0\3\25ui_elements_commands\1\20on_off_commands\1\19cursor_by_mode\1\17integrations\1\0\14\20nvim_bufferline\2\ttmux\1\18vim_gitgutter\1\rtwilight\2\17express_line\1\vfeline\1\14lightline\1\flualine\1\15galaxyline\1\16vim_signify\1\18vim_powerline\1\16vim_airline\1\rgitsigns\2\14limelight\1\nmodes\nfocus\1\0\2\20margin_of_error\3\5\17focus_method\17experimental\rataraxis\1\0\0\22affected_higroups\15SignColumn\17StatusLineNC\15StatusLine\14VertSplit\16ColorColumn\15FoldColumn\fNonText\1\0\0\29ideal_writing_area_width\1\2\0\0\3\0\1\0\b\17left_padding\3 \15custome_bg\5 keep_default_fold_fillchars\1\17auto_padding\1\19bottom_padding\3\1\16top_padding\3\1\18right_padding\3 \21bg_configuration\2\aui\1\0\0\tleft\1\0\3\vnumber\1\19relativenumber\1\15signcolumn\ano\btop\1\0\1\16showtabline\3\0\vbottom\1\0\0\1\0\5\fshowcmd\1\15laststatus\3\0\rshowmode\1\nruler\1\14cmdheight\3\1\nsetup\rtrue-zen\frequire\npcall\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/TrueZen.nvim"
  },
  ["bufferline.nvim"] = {
    config = { "\27LJ\2\nD\0\0\4\1\4\0\0066\0\0\0009\0\1\0009\0\2\0-\2\0\0'\3\3\0D\0\3\0\0¿\14term_type\21nvim_buf_get_var\bapi\bvimÄ\1\1\1\4\0\4\0\0266\1\0\0003\3\1\0B\1\2\3\15\0\1\0X\3\16Ä\a\2\2\0X\3\4Ä+\3\1\0002\0\0ÄL\3\2\0X\3\rÄ\a\2\3\0X\3\4Ä+\3\1\0002\0\0ÄL\3\2\0X\3\aÄ+\3\2\0002\0\0ÄL\3\2\0X\3\3Ä+\3\2\0002\0\0ÄL\3\2\0002\0\0ÄK\0\1\0\thori\tvert\0\npcall¯\f\1\0\t\0@\0ç\0016\0\0\0006\2\1\0'\3\2\0B\0\3\3\14\0\0\0X\2\1Ä2\0ÖÄ6\2\3\0009\2\4\2'\4\5\0B\2\2\0015\2\6\0009\3\a\0015\5\r\0005\6\t\0004\a\3\0005\b\b\0>\b\1\a=\a\n\0063\a\v\0=\a\f\6=\6\14\0055\6\20\0005\a\16\0009\b\15\2=\b\17\a9\b\18\2=\b\19\a=\a\21\0065\a\23\0009\b\22\2=\b\17\a9\b\24\2=\b\19\a=\a\25\0065\a\27\0009\b\26\2=\b\17\a9\b\18\2=\b\19\a=\a\28\0065\a\29\0009\b\26\2=\b\17\a9\b\18\2=\b\19\a=\a\30\0065\a\31\0009\b\26\2=\b\17\a9\b\18\2=\b\19\a=\a \0065\a!\0009\b\26\2=\b\17\a9\b\18\2=\b\19\a=\a\"\0065\a#\0009\b\26\2=\b\17\a9\b\18\2=\b\19\a=\a$\0065\a&\0009\b%\2=\b\17\a9\b\24\2=\b\19\a=\a'\0065\a(\0009\b\15\2=\b\17\a9\b\18\2=\b\19\a=\a)\0065\a*\0009\b\24\2=\b\17\a9\b\24\2=\b\19\a=\a+\0065\a,\0009\b%\2=\b\17\a9\b\18\2=\b\19\a=\a-\0065\a.\0009\b%\2=\b\17\a9\b\18\2=\b\19\a=\a/\0065\a1\0009\b0\2=\b\17\a9\b\24\2=\b\19\a=\a2\0065\a3\0009\b\18\2=\b\17\a9\b\18\2=\b\19\a=\a4\0065\a5\0009\b\18\2=\b\17\a9\b\18\2=\b\19\a=\a6\0065\a7\0009\b\18\2=\b\17\a9\b\18\2=\b\19\a=\a8\0065\a9\0009\b\15\2=\b\17\a9\b\26\2=\b\19\a=\a:\0065\a;\0009\b\18\2=\b\17\a9\b\15\2=\b\19\a=\a<\0065\a=\0009\b%\2=\b\17\a9\b\24\2=\b\19\a=\a>\6=\6?\5B\3\2\1K\0\1\0K\0\1\0\15highlights\14tab_close\1\0\0\17tab_selected\1\0\0\btab\1\0\0\23separator_selected\1\0\0\22separator_visible\1\0\0\14separator\1\0\0\22modified_selected\1\0\0\ngreen\21modified_visible\1\0\0\rmodified\1\0\0\23indicator_selected\1\0\0\tfill\1\0\0\26close_button_selected\1\0\0\bred\25close_button_visible\1\0\0\17close_button\1\0\0\21error_diagnostic\1\0\0\nerror\1\0\0\19buffer_visible\1\0\0\tgray\20buffer_selected\nblack\1\0\1\bgui\tbold\nwhite\15background\1\0\0\nguibg\vblack2\nguifg\1\0\0\afg\foptions\1\0\0\18custom_filter\0\foffsets\1\0\16\25enforce_regular_tabs\1\24show_tab_indicators\2\rtab_size\3\20\22max_prefix_length\3\r\20max_name_length\3\14\23right_trunc_marker\bÔÇ©\22left_trunc_marker\bÔÇ®\20show_close_icon\2\15close_icon\bÔåå\18modified_icon\bÔëÑ\22buffer_close_icon\bÔôï\16diagnostics\1\27always_show_bufferline\2\20separator_style\tthin\28show_buffer_close_icons\2\tview\16multiwindow\1\0\3\rfiletype\rNvimTree\ttext\5\fpadding\3\1\nsetup\1\0\15\nwhite\f#ECEFF4\afg\f#E5E9F0\vorange\f#D08770\abg\tNONE\vpurple\f#B48EAD\vblack2\f#3b4252\fmagenta\f#BF616A\nblack\f#2E3440\tgray\f#616E88\ngreen\f#8FBCBB\tblue\f#5E81AC\rdarkblue\f#81A1C1\bred\f#BF616A\tcyan\f#A3BE8C\vyellow\f#EBCB8BBfunction! Doom_bufferline_quitvim(a,b,c,d) \n qa \n endfunction\bcmd\bvim\15bufferline\frequire\npcall\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/bufferline.nvim"
  },
  ["cmp-buffer"] = {
    after_files = { "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp-buffer/after/plugin/cmp_buffer.lua" },
    load_after = {
      ["cmp-nvim-lsp"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp-buffer"
  },
  ["cmp-nvim-lsp"] = {
    after = { "cmp-buffer" },
    after_files = { "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lsp/after/plugin/cmp_nvim_lsp.lua" },
    load_after = {
      ["cmp-nvim-lua"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lsp"
  },
  ["cmp-nvim-lua"] = {
    after = { "cmp-nvim-lsp" },
    after_files = { "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lua/after/plugin/cmp_nvim_lua.lua" },
    load_after = {
      cmp_luasnip = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lua"
  },
  cmp_luasnip = {
    after = { "cmp-nvim-lua" },
    after_files = { "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp_luasnip/after/plugin/cmp_luasnip.lua", "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp_luasnip/after/plugin/cmp_luasnip.vim" },
    load_after = {
      LuaSnip = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/cmp_luasnip"
  },
  ["friendly-snippets"] = {
    after = { "nvim-cmp" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/friendly-snippets"
  },
  ["galaxyline.nvim"] = {
    config = { "\27LJ\2\n\20\0\0\1\0\1\0\2'\0\0\0L\0\2\0\t‚ñä ∆\3\0\0\6\1#\0J5\0\1\0-\1\0\0009\1\0\1=\1\2\0-\1\0\0009\1\3\1=\1\4\0-\1\0\0009\1\5\1=\1\6\0-\1\0\0009\1\a\1=\1\b\0-\1\0\0009\1\5\1=\1\t\0-\1\0\0009\1\n\1=\1\v\0-\1\0\0009\1\f\1=\1\r\0-\1\0\0009\1\f\1=\1\14\0-\1\0\0009\1\a\1=\1\15\0-\1\0\0009\1\a\1=\1\16\0-\1\0\0009\1\5\1=\1\17\0-\1\0\0009\1\n\1=\1\18\0-\1\0\0009\1\n\1=\1\19\0-\1\0\0009\1\20\1=\1\21\0-\1\0\0009\1\20\1=\1\22\0-\1\0\0009\1\0\1=\1\23\0-\1\0\0009\1\0\1=\1\24\0-\1\0\0009\1\0\1=\1\25\0-\1\0\0009\1\20\1=\1\26\0-\1\0\0009\1\20\1=\1\27\0006\1\28\0009\1\29\0019\1\30\1'\3\31\0006\4\28\0009\4 \0049\4!\4B\4\1\0028\4\4\0&\3\4\3B\1\2\1'\1\"\0L\1\2\0\5¿\t Œª \tmode\afn\27hi GalaxyViMode guifg=\17nvim_command\bapi\bvim\6t\6!\ar?\arm\6r\ace\acv\bred\aRv\6R\aic\6\19\6S\6s\ano\ngreen\6c\fmagenta\6V\6\22\vorange\6v\vyellow\6i\tblue\6n\1\0\0\tcyan\17\0\0\1\0\1\0\2'\0\0\0L\0\2\0\6 \17\0\0\1\0\1\0\2'\0\0\0L\0\2\0\6 \17\0\0\1\0\1\0\2'\0\0\0L\0\2\0\6 S\0\0\2\0\4\0\v5\0\0\0006\1\1\0009\1\2\0019\1\3\0018\1\1\0\15\0\1\0X\2\2Ä+\1\1\0L\1\2\0+\1\2\0L\1\2\0\rfiletype\abo\bvim\1\0\2\5\2\14dashboard\2\21\0\0\1\0\1\0\2'\0\0\0L\0\2\0\n Óú® «\23\1\0\r\0Ä\1\0÷\0026\0\0\0006\2\1\0'\3\2\0B\0\3\0036\2\0\0006\4\1\0'\5\3\0B\2\3\3\14\0\0\0X\4\3Ä\14\0\2\0X\4\1Ä2\0HÅ9\4\4\0015\5\6\0=\5\5\0015\5\a\0009\6\b\0045\a\15\0005\b\n\0003\t\t\0=\t\v\b4\t\3\0009\n\f\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b\16\a>\a\1\0069\6\b\0045\a\21\0005\b\18\0003\t\17\0=\t\v\b5\t\20\0009\n\19\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b\22\a>\a\2\0069\6\b\0045\a\25\0005\b\24\0003\t\23\0=\t\v\b=\b\26\a>\a\3\0069\6\b\0045\a\31\0005\b\27\0009\t\28\3=\t\29\b4\t\3\0009\n\30\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b \a>\a\4\0069\6\b\0045\a#\0005\b\"\0003\t!\0=\t\v\b=\b\26\a>\a\5\0069\6\b\0045\a'\0005\b$\0009\t\28\3=\t\29\b4\t\3\0006\n\1\0'\f%\0B\n\2\0029\n&\n>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b(\a>\a\6\0069\6\b\0045\a-\0005\b*\0005\t)\0=\t\v\b9\t\28\3=\t\29\b5\t,\0009\n+\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b.\a>\a\a\0069\6\b\0045\a1\0005\b0\0003\t/\0=\t\v\b=\b\26\a>\a\b\0069\6\b\0045\a5\0005\b2\0005\t3\0009\n\r\5>\n\2\t=\t4\b4\t\3\0009\n\30\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b6\a>\a\t\0069\6\b\0045\a;\0005\b7\0005\t8\0009\n\r\5>\n\2\t=\t4\b5\t:\0009\n9\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b<\a>\a\n\0069\6\b\0045\a>\0005\b=\0004\t\3\0009\n\19\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b?\a>\a\v\0069\6\b\0045\aB\0005\b@\0004\t\3\0009\nA\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bC\a>\a\f\0069\6\b\0045\aF\0005\bD\0004\t\3\0009\nE\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bG\a>\a\r\0069\6\b\0045\aI\0005\bH\0004\t\3\0009\n\f\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bJ\a>\a\14\0069\6K\0045\aP\0005\bL\0009\tM\3=\t\29\b5\tN\0009\n\r\5>\n\2\t=\t4\b5\tO\0009\n+\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bQ\a>\a\1\0069\6K\0045\aU\0005\bR\0003\tS\0=\t\29\b5\tT\0009\nA\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bV\a>\a\2\0069\6K\0045\aZ\0005\bW\0009\tM\3=\t\29\b5\tX\0009\n\r\5>\n\2\t=\t4\b5\tY\0009\n+\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b[\a>\a\3\0069\6K\0045\ab\0005\b]\0003\t\\\0=\t\v\b9\t^\3=\t\29\b5\t_\0009\n\r\5>\n\2\t=\t4\b5\ta\0009\n`\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bc\a>\a\4\0069\6K\0045\af\0005\bd\0009\t^\3=\t\29\b5\te\0009\n`\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bg\a>\a\5\0069\6K\0045\ai\0005\bh\0009\tM\3=\t\29\b4\t\3\0009\n+\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bj\a>\a\6\0069\6K\0045\am\0005\bk\0009\tM\3=\t\29\b4\t\3\0009\nl\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bn\a>\a\a\0069\6K\0045\ap\0005\bo\0009\tM\3=\t\29\b4\t\3\0009\n\19\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bq\a>\a\b\0069\6r\0045\av\0005\bs\0005\tt\0009\n\r\5>\n\2\t=\t4\b5\tu\0009\n\f\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bw\a>\a\1\0069\6r\0045\az\0005\bx\0009\t\28\3=\t\29\b5\ty\0009\n\30\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b{\a>\a\2\0069\6|\0045\a~\0005\b}\0004\t\3\0009\n\30\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b\a>\a\1\0062\0\0ÄK\0\1\0K\0\1\0\15BufferIcon\1\0\0\1\0\1\rprovider\15BufferIcon\21short_line_right\14SFileName\1\0\0\1\4\0\0\0\0\tbold\1\0\1\rprovider\14SFileName\15BufferType\1\0\0\1\4\0\0\0\0\tbold\1\2\0\0\tNONE\1\0\2\rprovider\17FileTypeName\14separator\6 \20short_line_left\15DiffRemove\1\0\0\1\0\2\rprovider\15DiffRemove\ticon\n ÔÖÜ \17DiffModified\1\0\0\vorange\1\0\2\rprovider\17DiffModified\ticon\t Ôßâ\fDiffAdd\1\0\0\1\0\2\rprovider\fDiffAdd\ticon\f  ÔÉæ  \14GitBranch\1\0\0\1\4\0\0\0\0\tbold\1\0\1\rprovider\14GitBranch\fGitIcon\1\0\0\1\4\0\0\0\0\tbold\fmagenta\1\2\0\0\tNONE\24check_git_workspace\1\0\1\14separator\6 \0\15FileFormat\1\0\0\1\4\0\0\0\0\tbold\1\2\0\0\tNONE\1\0\2\rprovider\15FileFormat\14separator\a  \18ShowLspClient\1\0\0\1\4\0\0\0\0\tbold\0\1\0\2\rprovider\17GetLspClient\ticon\b   \15FileEncode\1\0\0\1\4\0\0\0\0\tbold\1\2\0\0\tNONE\18hide_in_width\1\0\2\rprovider\15FileEncode\14separator\a  \nright\19DiagnosticInfo\1\0\0\1\0\2\ticon\n ÔÅö \rprovider\19DiagnosticInfo\19DiagnosticHint\1\0\0\tcyan\1\0\2\ticon\n ÔÅ™ \rprovider\19DiagnosticHint\19DiagnosticWarn\1\0\0\vyellow\1\0\2\ticon\n ÔÜà \rprovider\19DiagnosticWarn\20DiagnosticError\1\0\0\1\0\2\ticon\n ÔÜà \rprovider\20DiagnosticError\fPerCent\1\0\0\1\4\0\0\0\0\tbold\tgrey\1\2\0\0\tNONE\1\0\2\rprovider\16LinePercent\14separator\5\rLineInfo\1\0\0\24separator_highlight\1\2\0\0\tNONE\1\0\2\rprovider\15LineColumn\14separator\5\1\0\0\1\0\0\0\rFileName\1\0\0\1\4\0\0\0\0\tbold\ngreen\1\0\1\14separator\5\1\2\0\0\rFileName\rFileIcon\1\0\0\24get_file_icon_color!galaxyline.provider_fileinfo\1\0\1\rprovider\rFileIcon\1\0\0\1\0\0\0\rFileSize\1\0\0\afg\14condition\21buffer_not_empty\1\0\2\rprovider\rFileSize\14separator\5\nSpace\1\0\0\1\0\0\0\vViMode\1\0\0\1\4\0\0\0\0\tbold\bred\1\0\0\0\15RainbowRed\1\0\0\14highlight\abg\tblue\rprovider\1\0\0\0\tleft\1\0\r\nwhite\f#ECEFF4\afg\f#E5E9F0\fmagenta\f#BF616A\abg\tNONE\tgray\f#616E88\vpurple\f#B48EAD\tblue\f#5E81AC\vorange\f#D08770\bred\f#BF616A\ngreen\f#8FBCBB\rdarkblue\f#81A1C1\tcyan\f#A3BE8C\vyellow\f#EBCB8B\1\2\0\0\6 \20short_line_list\fsection\25galaxyline.condition\15galaxyline\frequire\npcall\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/galaxyline.nvim"
  },
  ["gitsigns.nvim"] = {
    config = { "\27LJ\2\n«\a\0\0\a\0\25\0\0316\0\0\0006\2\1\0'\3\2\0B\0\3\3\14\0\0\0X\2\1ÄK\0\1\0009\2\3\0015\4\t\0005\5\4\0005\6\5\0=\6\6\0055\6\a\0=\6\b\5=\5\n\0045\5\f\0005\6\v\0=\6\r\0055\6\14\0=\6\15\0055\6\16\0=\6\17\0055\6\18\0=\6\19\0055\6\20\0=\6\21\5=\5\22\0045\5\23\0=\5\24\4B\2\2\1K\0\1\0\16watch_index\1\0\1\rinterval\3d\nsigns\14topdelete\1\0\3\nnumhl\21GitSignsDeleteNr\ttext\b‚Äæ\ahl\15DiffDelete\vdelete\1\0\3\nnumhl\21GitSignsDeleteNr\ttext\6_\ahl\15DiffDelete\17changedelete\1\0\3\nnumhl\21GitSignsChangeNr\ttext\6~\ahl\15DiffChange\vchange\1\0\3\nnumhl\21GitSignsChangeNr\ttext\b‚îÇ\ahl\15DiffChange\badd\1\0\0\1\0\3\nnumhl\18GitSignsAddNr\ttext\b‚îÇ\ahl\fDiffAdd\fkeymaps\1\0\2\18sign_priority\3\5\nnumhl\1\tn [c\1\2\1\0@&diff ? '[c' : '<cmd>lua require\"gitsigns\".prev_hunk()<CR>'\texpr\2\tn ]c\1\2\1\0@&diff ? ']c' : '<cmd>lua require\"gitsigns\".next_hunk()<CR>'\texpr\2\1\0\a\vbuffer\2\17n <leader>hp2<cmd>lua require\"gitsigns\".preview_hunk()<CR>\fnoremap\2\17n <leader>hr0<cmd>lua require\"gitsigns\".reset_hunk()<CR>\17n <leader>hu5<cmd>lua require\"gitsigns\".undo_stage_hunk()<CR>\17n <leader>hs0<cmd>lua require\"gitsigns\".stage_hunk()<CR>\17n <leader>hb0<cmd>lua require\"gitsigns\".blame_line()<CR>\nsetup\rgitsigns\frequire\npcall\0" },
    loaded = true,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/start/gitsigns.nvim"
  },
  hop = {
    commands = { "HopWord", "HopLine", "HopChar1", "HopChar2", "HopPattern" },
    config = { "\27LJ\2\n1\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\bhop\frequire\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/hop"
  },
  ["indent-blankline.nvim"] = {
    config = { "\27LJ\2\n¶\2\0\0\4\0\b\0\v6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0005\3\4\0=\3\5\0025\3\6\0=\3\a\2B\0\2\1K\0\1\0\20buftype_exclude\1\2\0\0\rterminal\21filetype_exclude\1\b\0\0\thelp\rterminal\14dashboard\vpacker\flspinfo\20TelescopePrompt\21TelescopeResults\1\0\4\tchar\b‚ñè#show_trailing_blankline_indent\1\23indentLine_enabled\3\1\28show_first_indent_level\1\nsetup\21indent_blankline\frequire\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/indent-blankline.nvim"
  },
  ["lsp_signature.nvim"] = {
    config = { "\27LJ\2\nó\2\0\0\6\0\a\0\f6\0\0\0006\2\1\0'\3\2\0B\0\3\3\15\0\0\0X\2\5Ä9\2\3\0015\4\4\0005\5\5\0=\5\6\4B\2\2\1K\0\1\0\17handler_opts\1\0\1\vborder\vsingle\1\0\f\14max_width\3x\15max_height\3\22\vzindex\3»\1\17hi_parameter\vSearch\fpadding\5\16hint_scheme\vString\16hint_prefix\tÔüª \16hint_enable\2\ffix_pos\2\20floating_window\2\14doc_lines\3\2\tbind\2\nsetup\18lsp_signature\frequire\npcall\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/lsp_signature.nvim"
  },
  ["nord.nvim"] = {
    after = { "nvim-web-devicons" },
    config = { "\27LJ\2\n≈\1\0\0\3\0\b\0\0216\0\0\0009\0\1\0+\1\1\0=\1\2\0006\0\0\0009\0\1\0+\1\1\0=\1\3\0006\0\0\0009\0\1\0+\1\2\0=\1\4\0006\0\0\0009\0\1\0+\1\1\0=\1\5\0006\0\0\0009\0\6\0'\2\a\0B\0\2\1K\0\1\0\21colorscheme nord\bcmd\28nord_disable_background nord_cursorline_transparent\18nord_contrast\17nord_borders\6g\bvim\0" },
    loaded = true,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/nord.nvim"
  },
  ["nvim-autopairs"] = {
    config = { "\27LJ\2\n≠\1\0\0\a\0\6\0\0196\0\0\0006\2\1\0'\3\2\0B\0\3\0036\2\0\0006\4\1\0'\5\3\0B\2\3\3\14\0\0\0X\4\3Ä\14\0\2\0X\4\1ÄK\0\1\0009\4\4\1B\4\1\0019\4\4\0035\6\5\0B\4\2\1K\0\1\0\1\0\2\vmap_cr\2\17map_complete\2\nsetup\"nvim-autopairs.completion.cmp\19nvim-autopairs\frequire\npcall\0" },
    load_after = {
      ["nvim-cmp"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-autopairs"
  },
  ["nvim-cmp"] = {
    after = { "LuaSnip", "nvim-autopairs" },
    config = { "\27LJ\2\nC\0\1\4\0\4\0\a6\1\0\0'\3\1\0B\1\2\0029\1\2\0019\3\3\0B\1\2\1K\0\1\0\tbody\15lsp_expand\fluasnip\frequire^\0\2\4\0\4\0\0065\2\1\0009\3\2\0009\3\3\0038\2\3\2=\2\0\1L\1\2\0\tname\vsource\1\0\3\rnvim_lsp\n[LSP]\vbuffer\n[BUF]\rnvim_lua\n[Lua]\tmenu∑\2\0\1\t\0\r\1,6\1\0\0009\1\1\0019\1\2\1B\1\1\2\t\1\0\0X\1\14Ä6\1\0\0009\1\1\0019\1\3\0016\3\0\0009\3\4\0039\3\5\3'\5\6\0+\6\2\0+\a\2\0+\b\2\0B\3\5\2'\4\a\0B\1\3\1X\1\23Ä6\1\b\0'\3\t\0B\1\2\0029\1\n\1B\1\1\2\15\0\1\0X\2\14Ä6\1\0\0009\1\1\0019\1\3\0016\3\0\0009\3\4\0039\3\5\3'\5\v\0+\6\2\0+\a\2\0+\b\2\0B\3\5\2'\4\f\0B\1\3\1X\1\2Ä\18\1\0\0B\1\1\1K\0\1\0\5!<Plug>luasnip-expand-or-jump\23expand_or_jumpable\fluasnip\frequire\6n\n<C-n>\27nvim_replace_termcodes\bapi\rfeedkeys\15pumvisible\afn\bvim\2¨\2\0\1\t\0\r\1-6\1\0\0009\1\1\0019\1\2\1B\1\1\2\t\1\0\0X\1\14Ä6\1\0\0009\1\1\0019\1\3\0016\3\0\0009\3\4\0039\3\5\3'\5\6\0+\6\2\0+\a\2\0+\b\2\0B\3\5\2'\4\a\0B\1\3\1X\1\24Ä6\1\b\0'\3\t\0B\1\2\0029\1\n\1)\3ˇˇB\1\2\2\15\0\1\0X\2\14Ä6\1\0\0009\1\1\0019\1\3\0016\3\0\0009\3\4\0039\3\5\3'\5\v\0+\6\2\0+\a\2\0+\b\2\0B\3\5\2'\4\f\0B\1\3\1X\1\2Ä\18\1\0\0B\1\1\1K\0\1\0\5\28<Plug>luasnip-jump-prev\rjumpable\fluasnip\frequire\6n\n<C-p>\27nvim_replace_termcodes\bapi\rfeedkeys\15pumvisible\afn\bvim\2≤\5\1\0\n\0.\0L6\0\0\0006\2\1\0'\3\2\0B\0\3\3\14\0\0\0X\2\1Ä2\0DÄ6\2\3\0009\2\4\2'\3\6\0=\3\5\0029\2\a\0015\4\v\0005\5\t\0003\6\b\0=\6\n\5=\5\f\0045\5\14\0003\6\r\0=\6\15\5=\5\16\0045\5\19\0009\6\17\0019\6\18\6B\6\1\2=\6\20\0059\6\17\0019\6\21\6B\6\1\2=\6\22\0059\6\17\0019\6\23\6)\b¸ˇB\6\2\2=\6\24\0059\6\17\0019\6\23\6)\b\4\0B\6\2\2=\6\25\0059\6\17\0019\6\26\6B\6\1\2=\6\27\0059\6\17\0019\6\28\6B\6\1\2=\6\29\0059\6\17\0019\6\30\0065\b!\0009\t\31\0019\t \t=\t\"\bB\6\2\2=\6#\0053\6$\0=\6%\0053\6&\0=\6'\5=\5\17\0044\5\6\0005\6(\0>\6\1\0055\6)\0>\6\2\0055\6*\0>\6\3\0055\6+\0>\6\4\0055\6,\0>\6\5\5=\5-\4B\2\2\1K\0\1\0K\0\1\0\fsources\1\0\1\tname\forgmode\1\0\1\tname\rnvim_lua\1\0\1\tname\vbuffer\1\0\1\tname\fluasnip\1\0\1\tname\rnvim_lsp\f<S-Tab>\0\n<Tab>\0\t<CR>\rbehavior\1\0\1\vselect\2\fReplace\20ConfirmBehavior\fconfirm\n<C-e>\nclose\14<C-Space>\rcomplete\n<C-f>\n<C-d>\16scroll_docs\n<C-n>\21select_next_item\n<C-p>\1\0\0\21select_prev_item\fmapping\15formatting\vformat\1\0\0\0\fsnippet\1\0\0\vexpand\1\0\0\0\nsetup\21menuone,noselect\16completeopt\bopt\bvim\bcmp\frequire\npcall\0" },
    load_after = {
      ["friendly-snippets"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-cmp"
  },
  ["nvim-colorizer.lua"] = {
    config = { "\27LJ\2\n◊\1\0\0\6\0\t\0\0156\0\0\0006\2\1\0'\3\2\0B\0\3\3\15\0\0\0X\2\bÄ9\2\3\0015\4\4\0005\5\5\0B\2\3\0016\2\6\0009\2\a\2'\4\b\0B\2\2\1K\0\1\0\30ColorizerReloadAllBuffers\bcmd\bvim\1\0\t\vhsl_fn\1\tmode\15background\vcss_fn\1\bcss\1\vrgb_fn\1\rRRGGBBAA\1\nnames\1\vRRGGBB\2\bRGB\2\1\2\0\0\6*\nsetup\14colorizer\frequire\npcall\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-colorizer.lua"
  },
  ["nvim-lspconfig"] = {
    after = { "lsp_signature.nvim" },
    config = { "\27LJ\2\nA\2\0\4\1\3\0\a6\0\0\0009\0\1\0009\0\2\0-\2\0\0G\3\0\0A\0\1\1K\0\1\0\1¿\24nvim_buf_set_keymap\bapi\bvimA\2\0\4\1\3\0\a6\0\0\0009\0\1\0009\0\2\0-\2\0\0G\3\0\0A\0\1\1K\0\1\0\1¿\24nvim_buf_set_option\bapi\bvimò\v\1\2\v\0*\0u3\2\0\0003\3\1\0\18\4\3\0'\6\2\0'\a\3\0B\4\3\0015\4\4\0\18\5\2\0'\a\5\0'\b\6\0'\t\a\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\b\0'\t\t\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\n\0'\t\v\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\f\0'\t\r\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\14\0'\t\15\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\16\0'\t\17\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\18\0'\t\19\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\20\0'\t\21\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\22\0'\t\23\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\24\0'\t\25\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\26\0'\t\27\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\28\0'\t\29\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\30\0'\t\31\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b \0'\t!\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\"\0'\t#\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b$\0'\t%\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b&\0'\t'\0\18\n\4\0B\5\5\1\18\5\2\0'\a(\0'\b\26\0'\t)\0\18\n\4\0B\5\5\0012\0\0ÄK\0\1\0001<cmd>lua vim.lsp.buf.range_code_action()<CR>\6v*<cmd>lua vim.lsp.buf.formatting()<CR>\r<space>f2<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>\r<space>q0<cmd>lua vim.lsp.diagnostic.goto_next()<CR>\a]d0<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>\a[d<<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>\r<space>e*<cmd>lua vim.lsp.buf.references()<CR>\agr+<cmd>lua vim.lsp.buf.code_action()<CR>\14<space>ca&<cmd>lua vim.lsp.buf.rename()<CR>\14<space>rn/<cmd>lua vim.lsp.buf.type_definition()<CR>\r<space>DJ<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>\14<space>wl7<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>\14<space>wr4<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>\14<space>wa.<cmd>lua vim.lsp.buf.signature_help()<CR>\agk.<cmd>lua vim.lsp.buf.implementation()<CR>\agi%<cmd>lua vim.lsp.buf.hover()<CR>\6K*<cmd>lua vim.lsp.buf.definition()<CR>\agd+<cmd>lua vim.lsp.buf.declaration()<CR>\agD\6n\1\0\2\fnoremap\2\vsilent\2\27v:lua.vim.lsp.omnifunc\romnifunc\0\0–\4\0\0\16\4\29\0D-\0\0\0009\0\0\0B\0\1\1-\0\0\0009\0\1\0B\0\1\0026\1\2\0\18\3\0\0B\1\2\4H\0047Ä\6\5\3\0X\6\fÄ-\6\1\0008\6\5\0069\6\0\0065\b\4\0-\t\2\0=\t\5\b-\t\3\0=\t\6\b5\t\a\0=\t\b\bB\6\2\1X\6)Ä\a\5\3\0X\6'Ä-\6\1\0008\6\5\0069\6\0\0065\b\t\0-\t\2\0=\t\5\b-\t\3\0=\t\6\b5\t\n\0=\t\b\b5\t\26\0005\n\14\0005\v\f\0005\f\v\0=\f\r\v=\v\15\n5\v\21\0004\f\0\b6\r\16\0009\r\17\r9\r\18\r'\15\19\0B\r\2\2+\14\2\0<\14\r\f6\r\16\0009\r\17\r9\r\18\r'\15\20\0B\r\2\2+\14\2\0<\14\r\f=\f\22\v=\v\23\n5\v\24\0=\v\25\n=\n\27\t=\t\28\bB\6\2\1F\4\3\3R\4«K\0\1\0\3¿\1¿\4¿\5¿\rsettings\bLua\1\0\0\14telemetry\1\0\1\venable\1\14workspace\flibrary\1\0\2\20preloadFileSize\3êN\15maxPreload\3†ç\6\28$VIMRUNTIME/lua/vim/lsp\20$VIMRUNTIME/lua\vexpand\afn\bvim\16diagnostics\1\0\0\fglobals\1\0\0\1\2\0\0\bvim\1\0\1\26debounce_text_changes\3Ù\3\1\0\0\nflags\1\0\1\26debounce_text_changes\3Ù\3\17capabilities\14on_attach\1\0\0\blua\npairs\22installed_servers\nsetup5\0\0\3\1\3\0\a-\0\0\0B\0\1\0016\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0\6¿\fbufdo e\bcmd\bvimà\1\0\2\b\0\b\0\0146\2\0\0009\2\1\0029\2\2\2'\4\3\0\18\5\0\0&\4\5\0045\5\4\0=\1\5\5'\6\6\0\18\a\0\0&\6\a\6=\6\a\5B\2\3\1K\0\1\0\nnumhl\25LspDiagnosticsDefaul\ttext\1\0\0\23LspDiagnosticsSign\16sign_define\afn\bvim√\1\0\3\b\0\t\0\30\18\5\0\0009\3\0\0'\6\1\0B\3\3\2\15\0\3\0X\4\1ÄK\0\1\0006\3\2\0009\3\3\0039\3\4\0039\3\5\3\5\1\3\0X\3\6Ä6\3\2\0009\3\6\0039\3\a\3\18\5\0\0B\3\2\1X\3\nÄ6\3\2\0009\3\6\0039\3\b\0034\5\3\0004\6\3\0>\0\1\6>\6\1\5+\6\2\0004\a\0\0B\3\4\1K\0\1\0\14nvim_echo\21nvim_err_writeln\bapi\nERROR\vlevels\blog\bvim\14exit code\nmatchÛ\t\1\0\14\0008\0â\0016\0\0\0006\2\1\0'\3\2\0B\0\3\0036\2\0\0006\4\1\0'\5\3\0B\2\3\3\14\0\0\0X\4\3Ä\14\0\2\0X\4\1Ä2\0{Ä3\4\4\0006\5\5\0009\5\6\0059\5\a\0059\5\b\5B\5\1\0029\6\t\0059\6\n\0069\6\v\0065\a\r\0=\a\f\0069\6\t\0059\6\n\0069\6\v\6+\a\2\0=\a\14\0069\6\t\0059\6\n\0069\6\v\6+\a\2\0=\a\15\0069\6\t\0059\6\n\0069\6\v\6+\a\2\0=\a\16\0069\6\t\0059\6\n\0069\6\v\6+\a\2\0=\a\17\0069\6\t\0059\6\n\0069\6\v\6+\a\2\0=\a\18\0069\6\t\0059\6\n\0069\6\v\6+\a\2\0=\a\19\0069\6\t\0059\6\n\0069\6\v\0065\a\22\0005\b\21\0=\b\23\a=\a\20\0069\6\t\0059\6\n\0069\6\v\0065\a\26\0005\b\25\0=\b\27\a=\a\24\0063\6\28\0\18\a\6\0B\a\1\0013\a\30\0=\a\29\0033\a\31\0\18\b\a\0'\n \0'\v!\0B\b\3\1\18\b\a\0'\n\"\0'\v#\0B\b\3\1\18\b\a\0'\n$\0'\v%\0B\b\3\1\18\b\a\0'\n&\0'\v'\0B\b\3\0016\b\5\0009\b\6\b9\b(\b6\t\5\0009\t\6\t9\t*\t6\v\5\0009\v\6\v9\v+\v9\v,\v5\f.\0005\r-\0=\r/\fB\t\3\2=\t)\b6\b\5\0009\b\6\b9\b(\b6\t\5\0009\t\6\t9\t*\t6\v\5\0009\v\6\v9\v(\v9\v1\v5\f2\0B\t\3\2=\t0\b6\b\5\0009\b\6\b9\b(\b6\t\5\0009\t\6\t9\t*\t6\v\5\0009\v\6\v9\v(\v9\v4\v5\f5\0B\t\3\2=\t3\b6\b\5\0003\t7\0=\t6\b2\0\0ÄK\0\1\0K\0\1\0\0\vnotify\1\0\1\vborder\vsingle\19signature_help\31textDocument/signatureHelp\1\0\1\vborder\vsingle\nhover\23textDocument/hover\17virtual_text\1\0\3\14underline\2\nsigns\2\21update_in_insert\1\1\0\2\vprefix\bÔëÖ\fspacing\3\0\27on_publish_diagnostics\15diagnostic\twith$textDocument/publishDiagnostics\rhandlers\bÔÅ±\fWarning\bÔ†¥\tHint\bÔüª\16Information\bÔôò\nError\0\0\22post_install_hook\0\15properties\1\0\0\1\4\0\0\18documentation\vdetail\24additionalTextEdits\19resolveSupport\rvalueSet\1\0\0\1\2\0\0\3\1\15tagSupport\28commitCharactersSupport\22deprecatedSupport\24labelDetailsSupport\25insertReplaceSupport\21preselectSupport\19snippetSupport\1\3\0\0\rmarkdown\14plaintext\24documentationFormat\19completionItem\15completion\17textDocument\29make_client_capabilities\rprotocol\blsp\bvim\0\15lspinstall\14lspconfig\frequire\npcall\0" },
    loaded = true,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-lspconfig"
  },
  ["nvim-lspinstall"] = {
    loaded = true,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/start/nvim-lspinstall"
  },
  ["nvim-tree.lua"] = {
    commands = { "NvimTreeToggle", "NvimTreeFocus" },
    config = { "\27LJ\2\nâ\22\0\0\f\0q\0ã\0026\0\0\0006\2\1\0'\3\2\0B\0\3\3\14\0\0\0X\2\1ÄK\0\1\0005\2\3\0009\3\4\0016\4\5\0009\4\6\0046\5\5\0009\5\a\5+\6\2\0=\6\b\5)\5\0\0=\5\t\4)\5\1\0=\5\n\4)\5\0\0=\5\v\0045\5\r\0=\5\f\4)\5\0\0=\5\14\4)\5\1\0=\5\15\4)\5\1\0=\5\16\4)\5\1\0=\5\17\4)\5\1\0=\5\18\4)\5\0\0=\5\19\4)\5\0\0=\5\20\4)\5\0\0=\5\21\4)\5\1\0=\5\22\0045\5\24\0=\5\23\4)\5\0\0=\5\25\0046\5\27\0009\5\28\0055\a\29\0006\b\30\0009\b\31\b'\n \0)\vË\3B\b\3\2>\b\2\aB\5\2\2=\5\26\4'\5\"\0=\5!\4)\5\0\0=\5#\4)\5\1\0=\5$\4)\5\25\0=\5%\4)\5\0\0=\5&\0045\5(\0=\5'\0045\5*\0005\6+\0=\6,\0055\6-\0=\6.\5=\5)\0044\5 \0005\0061\0005\a0\0=\a2\6\18\a\3\0'\t3\0B\a\2\2=\a4\6>\6\1\0055\0066\0005\a5\0=\a2\6\18\a\3\0'\t7\0B\a\2\2=\a4\6>\6\2\0055\0068\0\18\a\3\0'\t9\0B\a\2\2=\a4\6>\6\3\0055\6:\0\18\a\3\0'\t;\0B\a\2\2=\a4\6>\6\4\0055\6<\0\18\a\3\0'\t=\0B\a\2\2=\a4\6>\6\5\0055\6>\0\18\a\3\0'\t?\0B\a\2\2=\a4\6>\6\6\0055\6@\0\18\a\3\0'\tA\0B\a\2\2=\a4\6>\6\a\0055\6B\0\18\a\3\0'\tC\0B\a\2\2=\a4\6>\6\b\0055\6D\0\18\a\3\0'\tE\0B\a\2\2=\a4\6>\6\t\0055\6F\0\18\a\3\0'\tE\0B\a\2\2=\a4\6>\6\n\0055\6G\0\18\a\3\0'\tH\0B\a\2\2=\a4\6>\6\v\0055\6I\0\18\a\3\0'\tJ\0B\a\2\2=\a4\6>\6\f\0055\6K\0\18\a\3\0'\tL\0B\a\2\2=\a4\6>\6\r\0055\6M\0\18\a\3\0'\tN\0B\a\2\2=\a4\6>\6\14\0055\6O\0\18\a\3\0'\tP\0B\a\2\2=\a4\6>\6\15\0055\6Q\0\18\a\3\0'\tR\0B\a\2\2=\a4\6>\6\16\0055\6S\0\18\a\3\0'\tT\0B\a\2\2=\a4\6>\6\17\0055\6U\0\18\a\3\0'\tV\0B\a\2\2=\a4\6>\6\18\0055\6W\0\18\a\3\0'\tX\0B\a\2\2=\a4\6>\6\19\0055\6Y\0\18\a\3\0'\tZ\0B\a\2\2=\a4\6>\6\20\0055\6[\0\18\a\3\0'\t\\\0B\a\2\2=\a4\6>\6\21\0055\6]\0\18\a\3\0'\t^\0B\a\2\2=\a4\6>\6\22\0055\6_\0\18\a\3\0'\t`\0B\a\2\2=\a4\6>\6\23\0055\6a\0\18\a\3\0'\tb\0B\a\2\2=\a4\6>\6\24\0055\6c\0\18\a\3\0'\td\0B\a\2\2=\a4\6>\6\25\0055\6e\0\18\a\3\0'\tf\0B\a\2\2=\a4\6>\6\26\0055\6g\0\18\a\3\0'\th\0B\a\2\2=\a4\6>\6\27\0055\6i\0\18\a\3\0'\tj\0B\a\2\2=\a4\6>\6\28\0055\6k\0\18\a\3\0'\tl\0B\a\2\2=\a4\6>\6\29\0055\6m\0\18\a\3\0'\tn\0B\a\2\2=\a4\6>\6\30\0055\6o\0\18\a\3\0'\tp\0B\a\2\2=\a4\6>\6\31\5=\5/\4K\0\1\0\16toggle_help\1\0\1\bkey\ag?\nclose\1\0\1\bkey\6q\vdir_up\1\0\1\bkey\6-\18next_git_item\1\0\1\bkey\a}c\18prev_git_item\1\0\1\bkey\a[c\23copy_absolute_path\1\0\1\bkey\agy\14copy_path\1\0\1\bkey\6Y\14copy_name\1\0\1\bkey\6y\npaste\1\0\1\bkey\6p\tcopy\1\0\1\bkey\6c\bcut\1\0\1\bkey\6x\16full_rename\1\0\1\bkey\t<C->\vrename\1\0\1\bkey\6r\vremove\1\0\1\bkey\6d\vcreate\1\0\1\bkey\6a\frefresh\1\0\1\bkey\6R\20toggle_dotfiles\1\0\1\bkey\6H\19toggle_ignored\1\0\1\bkey\6I\17last_sibling\1\0\1\bkey\6J\18first_sibling\1\0\1\bkey\6K\fpreview\1\0\1\bkey\n<Tab>\1\0\1\bkey\v<S-CR>\15close_node\1\0\1\bkey\t<BS>\16parent_node\1\0\1\bkey\6P\17next_sibling\1\0\1\bkey\6>\17prev_sibling\1\0\1\bkey\6<\vtabnew\1\0\1\bkey\n<C-t>\nsplit\1\0\1\bkey\n<C-x>\vvsplit\1\0\1\bkey\n<C-v>\acd\1\0\0\1\3\0\0\19<2-RightMouse>\n<C-]>\acb\tedit\bkey\1\0\0\1\4\0\0\t<CR>\6o\18<2-LeftMouse>\23nvim_tree_bindings\vfolder\1\0\6\15empty_open\bÔêî\nempty\bÔêì\fsymlink\bÔíÇ\fdefault\bÔêì\17symlink_open\bÓóæ\topen\bÔêì\bgit\1\0\a\fdeleted\bÔëò\runmerged\bÓúß\vstaged\b‚úì\runstaged\b‚úó\frenamed\b‚ûú\14untracked\b‚òÖ\fignored\b‚óå\1\0\2\fdefault\bÓòí\fsymlink\bÔíÅ\20nvim_tree_icons\1\0\3\nfiles\3\1\ffolders\3\1\bgit\3\1\25nvim_tree_show_icons\30nvim_tree_lsp_diagnostics\20nvim_tree_width\25nvim_tree_update_cwd\23nvim_tree_tab_open\tleft\19nvim_tree_side\6 \brep\vstring\1\4\0\0\16:t:gs?$?/..\0\r?:gs?^??\vconcat\ntable#nvim_tree_root_folder_modifier\27nvim_tree_quit_on_open\1\4\0\0\t.git\17node_modules\v.cache\21nvim_tree_ignore\29nvim_tree_indent_markers\27nvim_tree_hijack_netrw%nvim_tree_highlight_opened_files\28nvim_tree_hide_dotfiles\24nvim_tree_gitignore\21nvim_tree_git_hl\21nvim_tree_follow\28nvim_tree_disable_netrw\24nvim_tree_auto_open\1\2\0\0\14dashboard\29nvim_tree_auto_ignore_ft\25nvim_tree_auto_close\27nvim_tree_allow_resize\27nvim_tree_add_trailing\18termguicolors\6o\6g\bvim\23nvim_tree_callback\1\0\r\nwhite\f#ECEFF4\afg\f#E5E9F0\fmagenta\f#BF616A\abg\tNONE\tgray\f#616E88\vpurple\f#B48EAD\tblue\f#5E81AC\vorange\f#D08770\bred\f#BF616A\ngreen\f#8FBCBB\rdarkblue\f#81A1C1\tcyan\f#A3BE8C\vyellow\f#EBCB8B\21nvim-tree.config\frequire\npcall\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    config = { "\27LJ\2\n¶\1\0\0\6\0\a\0\r6\0\0\0006\2\1\0'\3\2\0B\0\3\3\14\0\0\0X\2\1ÄK\0\1\0009\2\3\0015\4\4\0005\5\5\0=\5\6\4B\2\2\1K\0\1\0\14highlight\1\0\2\21use_languagetree\2\venable\2\1\0\1\21ensure_installed\blua\nsetup\28nvim-treesitter.configs\frequire\npcall\0" },
    loaded = false,
    needs_bufread = true,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-treesitter"
  },
  ["nvim-web-devicons"] = {
    after = { "galaxyline.nvim", "bufferline.nvim" },
    config = { "\27LJ\2\n∞\n\0\0\t\0B\0j6\0\0\0006\2\1\0'\3\2\0B\0\3\3\14\0\0\0X\2\1ÄK\0\1\0005\2\3\0009\3\4\0015\5@\0005\6\b\0005\a\5\0009\b\6\2=\b\a\a=\a\t\0065\a\n\0009\b\6\2=\b\a\a=\a\v\0065\a\f\0009\b\r\2=\b\a\a=\a\14\0065\a\15\0009\b\r\2=\b\a\a=\a\16\0065\a\17\0009\b\18\2=\b\a\a=\a\19\0065\a\20\0009\b\21\2=\b\a\a=\a\22\0065\a\23\0009\b\21\2=\b\a\a=\a\24\0065\a\25\0009\b\26\2=\b\a\a=\a\27\0065\a\28\0009\b\29\2=\b\a\a=\a\30\0065\a\31\0009\b \2=\b\a\a=\a!\0065\a\"\0009\b\6\2=\b\a\a=\a#\0065\a$\0009\b%\2=\b\a\a=\a&\0065\a'\0009\b%\2=\b\a\a=\a(\0065\a)\0009\b%\2=\b\a\a=\a*\0065\a+\0009\b\21\2=\b\a\a=\a,\0065\a-\0009\b\r\2=\b\a\a=\a.\0065\a/\0009\b\6\2=\b\a\a=\a0\0065\a1\0009\b2\2=\b\a\a=\a3\0065\a4\0009\b5\2=\b\a\a=\a6\0065\a7\0009\b\29\2=\b\a\a=\a8\0065\a9\0009\b:\2=\b\a\a=\a;\0065\a<\0009\b\26\2=\b\a\a=\a=\0065\a>\0009\b\26\2=\b\a\a=\a?\6=\6A\5B\3\2\1K\0\1\0\roverride\1\0\0\bzip\1\0\2\ticon\bÔáÜ\tname\bzip\axz\1\0\2\ticon\bÔáÜ\tname\axz\bvue\18vibrant_green\1\0\2\ticon\bÔµÇ\tname\bvue\brpm\1\0\2\ticon\bÔåñ\tname\brpm\arb\tpink\1\0\2\ticon\bÓòÖ\tname\arb\ats\tteal\1\0\2\ticon\bÔØ§\tname\ats\ttoml\1\0\2\ticon\bÔÄì\tname\ttoml\apy\1\0\2\ticon\bÓòÜ\tname\apy\bpng\1\0\2\ticon\bÔùé\tname\bpng\bout\1\0\2\ticon\bÓòí\tname\bout\bmp4\1\0\2\ticon\bÔÖÑ\tname\bmp4\bmp3\nwhite\1\0\2\ticon\bÔ¢Ö\tname\bmp3\blua\1\0\2\ticon\bÓò†\tname\blua\tlock\bred\1\0\2\ticon\bÔ†Ω\tname\tlock\akt\vorange\1\0\2\ticon\tÛ±àô\tname\akt\ajs\bsun\1\0\2\ticon\bÔ†ù\tname\ajs\bjpg\1\0\2\ticon\bÔùé\tname\bjpg\tjpeg\16dark_purple\1\0\2\ticon\bÔùé\tname\tjpeg\thtml\14baby_pink\1\0\2\ticon\bÔÑª\tname\thtml\15Dockerfile\1\0\2\ticon\bÔåà\tname\15Dockerfile\bdeb\tcyan\1\0\2\ticon\bÔåÜ\tname\bdeb\bcss\1\0\2\ticon\bÓùâ\tname\bcss\6c\1\0\0\ncolor\tblue\1\0\2\ticon\bÓòû\tname\6c\nsetup\1\0\r\nwhite\f#ECEFF4\afg\f#E5E9F0\fmagenta\f#BF616A\abg\tNONE\tgray\f#616E88\vpurple\f#B48EAD\tblue\f#5E81AC\vorange\f#D08770\bred\f#BF616A\ngreen\f#8FBCBB\rdarkblue\f#81A1C1\tcyan\f#A3BE8C\vyellow\f#EBCB8B\22nvim-web-devicons\frequire\npcall\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/nvim-web-devicons"
  },
  ["orgmode.nvim"] = {
    config = { "\27LJ\2\n \v\0\0\6\0\16\0\0196\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0005\3\5\0005\4\4\0=\4\6\0035\4\a\0005\5\b\0=\5\t\4=\4\n\0035\4\v\0=\4\f\0035\4\r\0=\4\14\3=\3\15\2B\0\2\1K\0\1\0\rmappings\borg\1\0\24\15org_export\15<Leader>oe\25org_set_tags_command\15<Leader>ot!org_previous_visible_heading\6{\24org_archive_subtree\15<Leader>o$#org_forward_heading_same_level\a]]\21org_global_cycle\f<S-TAB>\14org_cycle\n<TAB>\29org_next_visible_heading\6}\22org_open_at_point\15<Leader>oo\24org_toggle_checkbox\14<C-Space>\22org_decrease_date\n<C-x>\26org_move_subtree_down\15<Leader>oJ\22org_increase_date\n<C-a>\24org_move_subtree_up\15<Leader>oK$org_backward_heading_same_level\a[[,org_insert_todo_heading_respect_content\16<Leader>oit\28org_insert_todo_heading\16<Leader>oiT'org_insert_heading_respect_content\16<Leader>oih\20org_meta_return\17<Leader><CR>\23org_demote_subtree\a>s\24org_promote_subtree\a<s\18org_do_demote\a>>\19org_do_promote\a<<\27org_toggle_archive_tag\15<Leader>oA\fcapture\1\0\4\21org_capture_kill\15<Leader>ok\23org_capture_refile\15<Leader>or\26org_capture_show_help\6?\25org_capture_finalize\n<C-c>\vagenda\20org_agenda_goto\1\2\0\0\n<TAB>\1\0\r\26org_agenda_month_view\avm\20org_agenda_redo\6r\25org_agenda_week_view\avw\20org_agenda_todo\6t\24org_agenda_day_view\avd\26org_agenda_goto_today\6.\23org_agenda_earlier\6b\21org_agenda_later\6f\25org_agenda_goto_date\6J\25org_agenda_show_help\6?\25org_agenda_switch_to\t<CR>\20org_agenda_quit\6q\25org_agenda_year_view\avy\vglobal\1\0\0\1\0\2\15org_agenda\15<Leader>oa\16org_capture\15<Leader>oc\1\0\5\27org_default_notes_file\20~/org/notes.org\21org_agenda_files\f~/org/*$org_highlight_latex_and_related\rentities\30org_hide_emphasis_markers\2\27org_hide_leading_stars\2\nsetup\forgmode\frequire\0" },
    loaded = false,
    needs_bufread = true,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["startuptime.vim"] = {
    commands = { "StartupTime" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/startuptime.vim"
  },
  ["telescope-fzf-native.nvim"] = {
    loaded = true,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/start/telescope-fzf-native.nvim"
  },
  ["telescope.nvim"] = {
    commands = { "Telescope" },
    config = { "\27LJ\2\nM\0\0\b\2\2\0\v6\0\0\0-\2\0\0B\0\2\4X\3\4Ä-\5\1\0009\5\1\5\18\a\4\0B\5\2\1E\3\3\3R\3˙K\0\1\0\2¿\1¿\19load_extension\vipairs¡\n\1\0\t\0,\0K6\0\0\0006\2\1\0'\3\2\0B\0\3\3\14\0\0\0X\2\1Ä2\0CÄ9\2\3\0015\4#\0005\5\5\0005\6\4\0=\6\6\0055\6\b\0005\a\a\0=\a\t\0065\a\n\0=\a\v\6=\6\f\0056\6\1\0'\b\r\0B\6\2\0029\6\14\6=\6\15\0054\6\0\0=\6\16\0056\6\1\0'\b\r\0B\6\2\0029\6\17\6=\6\18\0055\6\19\0=\6\20\0054\6\0\0=\6\21\0055\6\22\0=\6\23\0055\6\24\0=\6\25\0056\6\1\0'\b\26\0B\6\2\0029\6\27\0069\6\28\6=\6\29\0056\6\1\0'\b\26\0B\6\2\0029\6\30\0069\6\28\6=\6\31\0056\6\1\0'\b\26\0B\6\2\0029\6 \0069\6\28\6=\6!\0056\6\1\0'\b\26\0B\6\2\0029\6\"\6=\6\"\5=\5$\0045\5&\0005\6%\0=\6'\5=\5(\4B\2\2\0015\2)\0'\3*\0006\4\0\0003\6+\0B\4\2\0012\0\0ÄK\0\1\0K\0\1\0\0.\"extensions\", \"telescope-fzf-native.nvim\"\1\4\0\0\vthemes\nterms\bfzf\15extensions\bfzf\1\0\0\1\0\4\28override_generic_sorter\1\nfuzzy\2\14case_mode\15smart_case\25override_file_sorter\2\rdefaults\1\0\0\27buffer_previewer_maker\21qflist_previewer\22vim_buffer_qflist\19grep_previewer\23vim_buffer_vimgrep\19file_previewer\bnew\19vim_buffer_cat\25telescope.previewers\fset_env\1\0\1\14COLORTERM\14truecolor\16borderchars\1\t\0\0\b‚îÄ\b‚îÇ\b‚îÄ\b‚îÇ\b‚ï≠\b‚ïÆ\b‚ïØ\b‚ï∞\vborder\17path_display\1\2\0\0\rabsolute\19generic_sorter\29get_generic_fuzzy_sorter\25file_ignore_patterns\16file_sorter\19get_fuzzy_file\22telescope.sorters\18layout_config\rvertical\1\0\1\vmirror\1\15horizontal\1\0\3\19preview_cutoff\3x\vheight\4ö≥ÊÃ\tô≥¶ˇ\3\nwidth\4◊«¬Î\3äÆØˇ\3\1\0\3\18results_width\4ö≥ÊÃ\tô≥¶ˇ\3\18preview_width\4ö≥ÊÃ\tô≥Üˇ\3\20prompt_position\btop\22vimgrep_arguments\1\0\n\21sorting_strategy\14ascending\23selection_strategy\nreset\17initial_mode\vinsert\17entry_prefix\a  \19color_devicons\2\20selection_caret\b > \18prompt_prefix\t Œª \ruse_less\2\rwinblend\3\0\20layout_strategy\15horizontal\1\b\0\0\arg\18--color=never\17--no-heading\20--with-filename\18--line-number\r--column\17--smart-case\nsetup\14telescope\frequire\npcall\0" },
    loaded = false,
    needs_bufread = true,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/telescope.nvim"
  },
  ["twilight.nvim"] = {
    commands = { "Twilight", "TwilightEnable" },
    config = { "\27LJ\2\n:\0\0\3\0\3\0\a6\0\0\0'\2\1\0B\0\2\0029\0\2\0004\2\0\0B\0\2\1K\0\1\0\nsetup\rtwilight\frequire\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shauryasingh/.local/share/nvim/site/pack/packer/opt/twilight.nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: gitsigns.nvim
time([[Config for gitsigns.nvim]], true)
try_loadstring("\27LJ\2\n«\a\0\0\a\0\25\0\0316\0\0\0006\2\1\0'\3\2\0B\0\3\3\14\0\0\0X\2\1ÄK\0\1\0009\2\3\0015\4\t\0005\5\4\0005\6\5\0=\6\6\0055\6\a\0=\6\b\5=\5\n\0045\5\f\0005\6\v\0=\6\r\0055\6\14\0=\6\15\0055\6\16\0=\6\17\0055\6\18\0=\6\19\0055\6\20\0=\6\21\5=\5\22\0045\5\23\0=\5\24\4B\2\2\1K\0\1\0\16watch_index\1\0\1\rinterval\3d\nsigns\14topdelete\1\0\3\nnumhl\21GitSignsDeleteNr\ttext\b‚Äæ\ahl\15DiffDelete\vdelete\1\0\3\nnumhl\21GitSignsDeleteNr\ttext\6_\ahl\15DiffDelete\17changedelete\1\0\3\nnumhl\21GitSignsChangeNr\ttext\6~\ahl\15DiffChange\vchange\1\0\3\nnumhl\21GitSignsChangeNr\ttext\b‚îÇ\ahl\15DiffChange\badd\1\0\0\1\0\3\nnumhl\18GitSignsAddNr\ttext\b‚îÇ\ahl\fDiffAdd\fkeymaps\1\0\2\18sign_priority\3\5\nnumhl\1\tn [c\1\2\1\0@&diff ? '[c' : '<cmd>lua require\"gitsigns\".prev_hunk()<CR>'\texpr\2\tn ]c\1\2\1\0@&diff ? ']c' : '<cmd>lua require\"gitsigns\".next_hunk()<CR>'\texpr\2\1\0\a\vbuffer\2\17n <leader>hp2<cmd>lua require\"gitsigns\".preview_hunk()<CR>\fnoremap\2\17n <leader>hr0<cmd>lua require\"gitsigns\".reset_hunk()<CR>\17n <leader>hu5<cmd>lua require\"gitsigns\".undo_stage_hunk()<CR>\17n <leader>hs0<cmd>lua require\"gitsigns\".stage_hunk()<CR>\17n <leader>hb0<cmd>lua require\"gitsigns\".blame_line()<CR>\nsetup\rgitsigns\frequire\npcall\0", "config", "gitsigns.nvim")
time([[Config for gitsigns.nvim]], false)
-- Load plugins in order defined by `after`
time([[Sequenced loading]], true)
vim.cmd [[ packadd nord.nvim ]]

-- Config for: nord.nvim
try_loadstring("\27LJ\2\n≈\1\0\0\3\0\b\0\0216\0\0\0009\0\1\0+\1\1\0=\1\2\0006\0\0\0009\0\1\0+\1\1\0=\1\3\0006\0\0\0009\0\1\0+\1\2\0=\1\4\0006\0\0\0009\0\1\0+\1\1\0=\1\5\0006\0\0\0009\0\6\0'\2\a\0B\0\2\1K\0\1\0\21colorscheme nord\bcmd\28nord_disable_background nord_cursorline_transparent\18nord_contrast\17nord_borders\6g\bvim\0", "config", "nord.nvim")

vim.cmd [[ packadd nvim-web-devicons ]]

-- Config for: nvim-web-devicons
try_loadstring("\27LJ\2\n∞\n\0\0\t\0B\0j6\0\0\0006\2\1\0'\3\2\0B\0\3\3\14\0\0\0X\2\1ÄK\0\1\0005\2\3\0009\3\4\0015\5@\0005\6\b\0005\a\5\0009\b\6\2=\b\a\a=\a\t\0065\a\n\0009\b\6\2=\b\a\a=\a\v\0065\a\f\0009\b\r\2=\b\a\a=\a\14\0065\a\15\0009\b\r\2=\b\a\a=\a\16\0065\a\17\0009\b\18\2=\b\a\a=\a\19\0065\a\20\0009\b\21\2=\b\a\a=\a\22\0065\a\23\0009\b\21\2=\b\a\a=\a\24\0065\a\25\0009\b\26\2=\b\a\a=\a\27\0065\a\28\0009\b\29\2=\b\a\a=\a\30\0065\a\31\0009\b \2=\b\a\a=\a!\0065\a\"\0009\b\6\2=\b\a\a=\a#\0065\a$\0009\b%\2=\b\a\a=\a&\0065\a'\0009\b%\2=\b\a\a=\a(\0065\a)\0009\b%\2=\b\a\a=\a*\0065\a+\0009\b\21\2=\b\a\a=\a,\0065\a-\0009\b\r\2=\b\a\a=\a.\0065\a/\0009\b\6\2=\b\a\a=\a0\0065\a1\0009\b2\2=\b\a\a=\a3\0065\a4\0009\b5\2=\b\a\a=\a6\0065\a7\0009\b\29\2=\b\a\a=\a8\0065\a9\0009\b:\2=\b\a\a=\a;\0065\a<\0009\b\26\2=\b\a\a=\a=\0065\a>\0009\b\26\2=\b\a\a=\a?\6=\6A\5B\3\2\1K\0\1\0\roverride\1\0\0\bzip\1\0\2\ticon\bÔáÜ\tname\bzip\axz\1\0\2\ticon\bÔáÜ\tname\axz\bvue\18vibrant_green\1\0\2\ticon\bÔµÇ\tname\bvue\brpm\1\0\2\ticon\bÔåñ\tname\brpm\arb\tpink\1\0\2\ticon\bÓòÖ\tname\arb\ats\tteal\1\0\2\ticon\bÔØ§\tname\ats\ttoml\1\0\2\ticon\bÔÄì\tname\ttoml\apy\1\0\2\ticon\bÓòÜ\tname\apy\bpng\1\0\2\ticon\bÔùé\tname\bpng\bout\1\0\2\ticon\bÓòí\tname\bout\bmp4\1\0\2\ticon\bÔÖÑ\tname\bmp4\bmp3\nwhite\1\0\2\ticon\bÔ¢Ö\tname\bmp3\blua\1\0\2\ticon\bÓò†\tname\blua\tlock\bred\1\0\2\ticon\bÔ†Ω\tname\tlock\akt\vorange\1\0\2\ticon\tÛ±àô\tname\akt\ajs\bsun\1\0\2\ticon\bÔ†ù\tname\ajs\bjpg\1\0\2\ticon\bÔùé\tname\bjpg\tjpeg\16dark_purple\1\0\2\ticon\bÔùé\tname\tjpeg\thtml\14baby_pink\1\0\2\ticon\bÔÑª\tname\thtml\15Dockerfile\1\0\2\ticon\bÔåà\tname\15Dockerfile\bdeb\tcyan\1\0\2\ticon\bÔåÜ\tname\bdeb\bcss\1\0\2\ticon\bÓùâ\tname\bcss\6c\1\0\0\ncolor\tblue\1\0\2\ticon\bÓòû\tname\6c\nsetup\1\0\r\nwhite\f#ECEFF4\afg\f#E5E9F0\fmagenta\f#BF616A\abg\tNONE\tgray\f#616E88\vpurple\f#B48EAD\tblue\f#5E81AC\vorange\f#D08770\bred\f#BF616A\ngreen\f#8FBCBB\rdarkblue\f#81A1C1\tcyan\f#A3BE8C\vyellow\f#EBCB8B\22nvim-web-devicons\frequire\npcall\0", "config", "nvim-web-devicons")

vim.cmd [[ packadd bufferline.nvim ]]

-- Config for: bufferline.nvim
try_loadstring("\27LJ\2\nD\0\0\4\1\4\0\0066\0\0\0009\0\1\0009\0\2\0-\2\0\0'\3\3\0D\0\3\0\0¿\14term_type\21nvim_buf_get_var\bapi\bvimÄ\1\1\1\4\0\4\0\0266\1\0\0003\3\1\0B\1\2\3\15\0\1\0X\3\16Ä\a\2\2\0X\3\4Ä+\3\1\0002\0\0ÄL\3\2\0X\3\rÄ\a\2\3\0X\3\4Ä+\3\1\0002\0\0ÄL\3\2\0X\3\aÄ+\3\2\0002\0\0ÄL\3\2\0X\3\3Ä+\3\2\0002\0\0ÄL\3\2\0002\0\0ÄK\0\1\0\thori\tvert\0\npcall¯\f\1\0\t\0@\0ç\0016\0\0\0006\2\1\0'\3\2\0B\0\3\3\14\0\0\0X\2\1Ä2\0ÖÄ6\2\3\0009\2\4\2'\4\5\0B\2\2\0015\2\6\0009\3\a\0015\5\r\0005\6\t\0004\a\3\0005\b\b\0>\b\1\a=\a\n\0063\a\v\0=\a\f\6=\6\14\0055\6\20\0005\a\16\0009\b\15\2=\b\17\a9\b\18\2=\b\19\a=\a\21\0065\a\23\0009\b\22\2=\b\17\a9\b\24\2=\b\19\a=\a\25\0065\a\27\0009\b\26\2=\b\17\a9\b\18\2=\b\19\a=\a\28\0065\a\29\0009\b\26\2=\b\17\a9\b\18\2=\b\19\a=\a\30\0065\a\31\0009\b\26\2=\b\17\a9\b\18\2=\b\19\a=\a \0065\a!\0009\b\26\2=\b\17\a9\b\18\2=\b\19\a=\a\"\0065\a#\0009\b\26\2=\b\17\a9\b\18\2=\b\19\a=\a$\0065\a&\0009\b%\2=\b\17\a9\b\24\2=\b\19\a=\a'\0065\a(\0009\b\15\2=\b\17\a9\b\18\2=\b\19\a=\a)\0065\a*\0009\b\24\2=\b\17\a9\b\24\2=\b\19\a=\a+\0065\a,\0009\b%\2=\b\17\a9\b\18\2=\b\19\a=\a-\0065\a.\0009\b%\2=\b\17\a9\b\18\2=\b\19\a=\a/\0065\a1\0009\b0\2=\b\17\a9\b\24\2=\b\19\a=\a2\0065\a3\0009\b\18\2=\b\17\a9\b\18\2=\b\19\a=\a4\0065\a5\0009\b\18\2=\b\17\a9\b\18\2=\b\19\a=\a6\0065\a7\0009\b\18\2=\b\17\a9\b\18\2=\b\19\a=\a8\0065\a9\0009\b\15\2=\b\17\a9\b\26\2=\b\19\a=\a:\0065\a;\0009\b\18\2=\b\17\a9\b\15\2=\b\19\a=\a<\0065\a=\0009\b%\2=\b\17\a9\b\24\2=\b\19\a=\a>\6=\6?\5B\3\2\1K\0\1\0K\0\1\0\15highlights\14tab_close\1\0\0\17tab_selected\1\0\0\btab\1\0\0\23separator_selected\1\0\0\22separator_visible\1\0\0\14separator\1\0\0\22modified_selected\1\0\0\ngreen\21modified_visible\1\0\0\rmodified\1\0\0\23indicator_selected\1\0\0\tfill\1\0\0\26close_button_selected\1\0\0\bred\25close_button_visible\1\0\0\17close_button\1\0\0\21error_diagnostic\1\0\0\nerror\1\0\0\19buffer_visible\1\0\0\tgray\20buffer_selected\nblack\1\0\1\bgui\tbold\nwhite\15background\1\0\0\nguibg\vblack2\nguifg\1\0\0\afg\foptions\1\0\0\18custom_filter\0\foffsets\1\0\16\25enforce_regular_tabs\1\24show_tab_indicators\2\rtab_size\3\20\22max_prefix_length\3\r\20max_name_length\3\14\23right_trunc_marker\bÔÇ©\22left_trunc_marker\bÔÇ®\20show_close_icon\2\15close_icon\bÔåå\18modified_icon\bÔëÑ\22buffer_close_icon\bÔôï\16diagnostics\1\27always_show_bufferline\2\20separator_style\tthin\28show_buffer_close_icons\2\tview\16multiwindow\1\0\3\rfiletype\rNvimTree\ttext\5\fpadding\3\1\nsetup\1\0\15\nwhite\f#ECEFF4\afg\f#E5E9F0\vorange\f#D08770\abg\tNONE\vpurple\f#B48EAD\vblack2\f#3b4252\fmagenta\f#BF616A\nblack\f#2E3440\tgray\f#616E88\ngreen\f#8FBCBB\tblue\f#5E81AC\rdarkblue\f#81A1C1\bred\f#BF616A\tcyan\f#A3BE8C\vyellow\f#EBCB8BBfunction! Doom_bufferline_quitvim(a,b,c,d) \n qa \n endfunction\bcmd\bvim\15bufferline\frequire\npcall\0", "config", "bufferline.nvim")

vim.cmd [[ packadd galaxyline.nvim ]]

-- Config for: galaxyline.nvim
try_loadstring("\27LJ\2\n\20\0\0\1\0\1\0\2'\0\0\0L\0\2\0\t‚ñä ∆\3\0\0\6\1#\0J5\0\1\0-\1\0\0009\1\0\1=\1\2\0-\1\0\0009\1\3\1=\1\4\0-\1\0\0009\1\5\1=\1\6\0-\1\0\0009\1\a\1=\1\b\0-\1\0\0009\1\5\1=\1\t\0-\1\0\0009\1\n\1=\1\v\0-\1\0\0009\1\f\1=\1\r\0-\1\0\0009\1\f\1=\1\14\0-\1\0\0009\1\a\1=\1\15\0-\1\0\0009\1\a\1=\1\16\0-\1\0\0009\1\5\1=\1\17\0-\1\0\0009\1\n\1=\1\18\0-\1\0\0009\1\n\1=\1\19\0-\1\0\0009\1\20\1=\1\21\0-\1\0\0009\1\20\1=\1\22\0-\1\0\0009\1\0\1=\1\23\0-\1\0\0009\1\0\1=\1\24\0-\1\0\0009\1\0\1=\1\25\0-\1\0\0009\1\20\1=\1\26\0-\1\0\0009\1\20\1=\1\27\0006\1\28\0009\1\29\0019\1\30\1'\3\31\0006\4\28\0009\4 \0049\4!\4B\4\1\0028\4\4\0&\3\4\3B\1\2\1'\1\"\0L\1\2\0\5¿\t Œª \tmode\afn\27hi GalaxyViMode guifg=\17nvim_command\bapi\bvim\6t\6!\ar?\arm\6r\ace\acv\bred\aRv\6R\aic\6\19\6S\6s\ano\ngreen\6c\fmagenta\6V\6\22\vorange\6v\vyellow\6i\tblue\6n\1\0\0\tcyan\17\0\0\1\0\1\0\2'\0\0\0L\0\2\0\6 \17\0\0\1\0\1\0\2'\0\0\0L\0\2\0\6 \17\0\0\1\0\1\0\2'\0\0\0L\0\2\0\6 S\0\0\2\0\4\0\v5\0\0\0006\1\1\0009\1\2\0019\1\3\0018\1\1\0\15\0\1\0X\2\2Ä+\1\1\0L\1\2\0+\1\2\0L\1\2\0\rfiletype\abo\bvim\1\0\2\5\2\14dashboard\2\21\0\0\1\0\1\0\2'\0\0\0L\0\2\0\n Óú® «\23\1\0\r\0Ä\1\0÷\0026\0\0\0006\2\1\0'\3\2\0B\0\3\0036\2\0\0006\4\1\0'\5\3\0B\2\3\3\14\0\0\0X\4\3Ä\14\0\2\0X\4\1Ä2\0HÅ9\4\4\0015\5\6\0=\5\5\0015\5\a\0009\6\b\0045\a\15\0005\b\n\0003\t\t\0=\t\v\b4\t\3\0009\n\f\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b\16\a>\a\1\0069\6\b\0045\a\21\0005\b\18\0003\t\17\0=\t\v\b5\t\20\0009\n\19\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b\22\a>\a\2\0069\6\b\0045\a\25\0005\b\24\0003\t\23\0=\t\v\b=\b\26\a>\a\3\0069\6\b\0045\a\31\0005\b\27\0009\t\28\3=\t\29\b4\t\3\0009\n\30\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b \a>\a\4\0069\6\b\0045\a#\0005\b\"\0003\t!\0=\t\v\b=\b\26\a>\a\5\0069\6\b\0045\a'\0005\b$\0009\t\28\3=\t\29\b4\t\3\0006\n\1\0'\f%\0B\n\2\0029\n&\n>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b(\a>\a\6\0069\6\b\0045\a-\0005\b*\0005\t)\0=\t\v\b9\t\28\3=\t\29\b5\t,\0009\n+\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b.\a>\a\a\0069\6\b\0045\a1\0005\b0\0003\t/\0=\t\v\b=\b\26\a>\a\b\0069\6\b\0045\a5\0005\b2\0005\t3\0009\n\r\5>\n\2\t=\t4\b4\t\3\0009\n\30\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b6\a>\a\t\0069\6\b\0045\a;\0005\b7\0005\t8\0009\n\r\5>\n\2\t=\t4\b5\t:\0009\n9\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b<\a>\a\n\0069\6\b\0045\a>\0005\b=\0004\t\3\0009\n\19\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b?\a>\a\v\0069\6\b\0045\aB\0005\b@\0004\t\3\0009\nA\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bC\a>\a\f\0069\6\b\0045\aF\0005\bD\0004\t\3\0009\nE\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bG\a>\a\r\0069\6\b\0045\aI\0005\bH\0004\t\3\0009\n\f\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bJ\a>\a\14\0069\6K\0045\aP\0005\bL\0009\tM\3=\t\29\b5\tN\0009\n\r\5>\n\2\t=\t4\b5\tO\0009\n+\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bQ\a>\a\1\0069\6K\0045\aU\0005\bR\0003\tS\0=\t\29\b5\tT\0009\nA\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bV\a>\a\2\0069\6K\0045\aZ\0005\bW\0009\tM\3=\t\29\b5\tX\0009\n\r\5>\n\2\t=\t4\b5\tY\0009\n+\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b[\a>\a\3\0069\6K\0045\ab\0005\b]\0003\t\\\0=\t\v\b9\t^\3=\t\29\b5\t_\0009\n\r\5>\n\2\t=\t4\b5\ta\0009\n`\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bc\a>\a\4\0069\6K\0045\af\0005\bd\0009\t^\3=\t\29\b5\te\0009\n`\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bg\a>\a\5\0069\6K\0045\ai\0005\bh\0009\tM\3=\t\29\b4\t\3\0009\n+\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bj\a>\a\6\0069\6K\0045\am\0005\bk\0009\tM\3=\t\29\b4\t\3\0009\nl\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bn\a>\a\a\0069\6K\0045\ap\0005\bo\0009\tM\3=\t\29\b4\t\3\0009\n\19\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bq\a>\a\b\0069\6r\0045\av\0005\bs\0005\tt\0009\n\r\5>\n\2\t=\t4\b5\tu\0009\n\f\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\bw\a>\a\1\0069\6r\0045\az\0005\bx\0009\t\28\3=\t\29\b5\ty\0009\n\30\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b{\a>\a\2\0069\6|\0045\a~\0005\b}\0004\t\3\0009\n\30\5>\n\1\t9\n\r\5>\n\2\t=\t\14\b=\b\a>\a\1\0062\0\0ÄK\0\1\0K\0\1\0\15BufferIcon\1\0\0\1\0\1\rprovider\15BufferIcon\21short_line_right\14SFileName\1\0\0\1\4\0\0\0\0\tbold\1\0\1\rprovider\14SFileName\15BufferType\1\0\0\1\4\0\0\0\0\tbold\1\2\0\0\tNONE\1\0\2\rprovider\17FileTypeName\14separator\6 \20short_line_left\15DiffRemove\1\0\0\1\0\2\rprovider\15DiffRemove\ticon\n ÔÖÜ \17DiffModified\1\0\0\vorange\1\0\2\rprovider\17DiffModified\ticon\t Ôßâ\fDiffAdd\1\0\0\1\0\2\rprovider\fDiffAdd\ticon\f  ÔÉæ  \14GitBranch\1\0\0\1\4\0\0\0\0\tbold\1\0\1\rprovider\14GitBranch\fGitIcon\1\0\0\1\4\0\0\0\0\tbold\fmagenta\1\2\0\0\tNONE\24check_git_workspace\1\0\1\14separator\6 \0\15FileFormat\1\0\0\1\4\0\0\0\0\tbold\1\2\0\0\tNONE\1\0\2\rprovider\15FileFormat\14separator\a  \18ShowLspClient\1\0\0\1\4\0\0\0\0\tbold\0\1\0\2\rprovider\17GetLspClient\ticon\b   \15FileEncode\1\0\0\1\4\0\0\0\0\tbold\1\2\0\0\tNONE\18hide_in_width\1\0\2\rprovider\15FileEncode\14separator\a  \nright\19DiagnosticInfo\1\0\0\1\0\2\ticon\n ÔÅö \rprovider\19DiagnosticInfo\19DiagnosticHint\1\0\0\tcyan\1\0\2\ticon\n ÔÅ™ \rprovider\19DiagnosticHint\19DiagnosticWarn\1\0\0\vyellow\1\0\2\ticon\n ÔÜà \rprovider\19DiagnosticWarn\20DiagnosticError\1\0\0\1\0\2\ticon\n ÔÜà \rprovider\20DiagnosticError\fPerCent\1\0\0\1\4\0\0\0\0\tbold\tgrey\1\2\0\0\tNONE\1\0\2\rprovider\16LinePercent\14separator\5\rLineInfo\1\0\0\24separator_highlight\1\2\0\0\tNONE\1\0\2\rprovider\15LineColumn\14separator\5\1\0\0\1\0\0\0\rFileName\1\0\0\1\4\0\0\0\0\tbold\ngreen\1\0\1\14separator\5\1\2\0\0\rFileName\rFileIcon\1\0\0\24get_file_icon_color!galaxyline.provider_fileinfo\1\0\1\rprovider\rFileIcon\1\0\0\1\0\0\0\rFileSize\1\0\0\afg\14condition\21buffer_not_empty\1\0\2\rprovider\rFileSize\14separator\5\nSpace\1\0\0\1\0\0\0\vViMode\1\0\0\1\4\0\0\0\0\tbold\bred\1\0\0\0\15RainbowRed\1\0\0\14highlight\abg\tblue\rprovider\1\0\0\0\tleft\1\0\r\nwhite\f#ECEFF4\afg\f#E5E9F0\fmagenta\f#BF616A\abg\tNONE\tgray\f#616E88\vpurple\f#B48EAD\tblue\f#5E81AC\vorange\f#D08770\bred\f#BF616A\ngreen\f#8FBCBB\rdarkblue\f#81A1C1\tcyan\f#A3BE8C\vyellow\f#EBCB8B\1\2\0\0\6 \20short_line_list\fsection\25galaxyline.condition\15galaxyline\frequire\npcall\0", "config", "galaxyline.nvim")

vim.cmd [[ packadd nvim-lspconfig ]]

-- Config for: nvim-lspconfig
try_loadstring("\27LJ\2\nA\2\0\4\1\3\0\a6\0\0\0009\0\1\0009\0\2\0-\2\0\0G\3\0\0A\0\1\1K\0\1\0\1¿\24nvim_buf_set_keymap\bapi\bvimA\2\0\4\1\3\0\a6\0\0\0009\0\1\0009\0\2\0-\2\0\0G\3\0\0A\0\1\1K\0\1\0\1¿\24nvim_buf_set_option\bapi\bvimò\v\1\2\v\0*\0u3\2\0\0003\3\1\0\18\4\3\0'\6\2\0'\a\3\0B\4\3\0015\4\4\0\18\5\2\0'\a\5\0'\b\6\0'\t\a\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\b\0'\t\t\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\n\0'\t\v\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\f\0'\t\r\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\14\0'\t\15\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\16\0'\t\17\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\18\0'\t\19\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\20\0'\t\21\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\22\0'\t\23\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\24\0'\t\25\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\26\0'\t\27\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\28\0'\t\29\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\30\0'\t\31\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b \0'\t!\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b\"\0'\t#\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b$\0'\t%\0\18\n\4\0B\5\5\1\18\5\2\0'\a\5\0'\b&\0'\t'\0\18\n\4\0B\5\5\1\18\5\2\0'\a(\0'\b\26\0'\t)\0\18\n\4\0B\5\5\0012\0\0ÄK\0\1\0001<cmd>lua vim.lsp.buf.range_code_action()<CR>\6v*<cmd>lua vim.lsp.buf.formatting()<CR>\r<space>f2<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>\r<space>q0<cmd>lua vim.lsp.diagnostic.goto_next()<CR>\a]d0<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>\a[d<<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>\r<space>e*<cmd>lua vim.lsp.buf.references()<CR>\agr+<cmd>lua vim.lsp.buf.code_action()<CR>\14<space>ca&<cmd>lua vim.lsp.buf.rename()<CR>\14<space>rn/<cmd>lua vim.lsp.buf.type_definition()<CR>\r<space>DJ<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>\14<space>wl7<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>\14<space>wr4<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>\14<space>wa.<cmd>lua vim.lsp.buf.signature_help()<CR>\agk.<cmd>lua vim.lsp.buf.implementation()<CR>\agi%<cmd>lua vim.lsp.buf.hover()<CR>\6K*<cmd>lua vim.lsp.buf.definition()<CR>\agd+<cmd>lua vim.lsp.buf.declaration()<CR>\agD\6n\1\0\2\fnoremap\2\vsilent\2\27v:lua.vim.lsp.omnifunc\romnifunc\0\0–\4\0\0\16\4\29\0D-\0\0\0009\0\0\0B\0\1\1-\0\0\0009\0\1\0B\0\1\0026\1\2\0\18\3\0\0B\1\2\4H\0047Ä\6\5\3\0X\6\fÄ-\6\1\0008\6\5\0069\6\0\0065\b\4\0-\t\2\0=\t\5\b-\t\3\0=\t\6\b5\t\a\0=\t\b\bB\6\2\1X\6)Ä\a\5\3\0X\6'Ä-\6\1\0008\6\5\0069\6\0\0065\b\t\0-\t\2\0=\t\5\b-\t\3\0=\t\6\b5\t\n\0=\t\b\b5\t\26\0005\n\14\0005\v\f\0005\f\v\0=\f\r\v=\v\15\n5\v\21\0004\f\0\b6\r\16\0009\r\17\r9\r\18\r'\15\19\0B\r\2\2+\14\2\0<\14\r\f6\r\16\0009\r\17\r9\r\18\r'\15\20\0B\r\2\2+\14\2\0<\14\r\f=\f\22\v=\v\23\n5\v\24\0=\v\25\n=\n\27\t=\t\28\bB\6\2\1F\4\3\3R\4«K\0\1\0\3¿\1¿\4¿\5¿\rsettings\bLua\1\0\0\14telemetry\1\0\1\venable\1\14workspace\flibrary\1\0\2\20preloadFileSize\3êN\15maxPreload\3†ç\6\28$VIMRUNTIME/lua/vim/lsp\20$VIMRUNTIME/lua\vexpand\afn\bvim\16diagnostics\1\0\0\fglobals\1\0\0\1\2\0\0\bvim\1\0\1\26debounce_text_changes\3Ù\3\1\0\0\nflags\1\0\1\26debounce_text_changes\3Ù\3\17capabilities\14on_attach\1\0\0\blua\npairs\22installed_servers\nsetup5\0\0\3\1\3\0\a-\0\0\0B\0\1\0016\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0\6¿\fbufdo e\bcmd\bvimà\1\0\2\b\0\b\0\0146\2\0\0009\2\1\0029\2\2\2'\4\3\0\18\5\0\0&\4\5\0045\5\4\0=\1\5\5'\6\6\0\18\a\0\0&\6\a\6=\6\a\5B\2\3\1K\0\1\0\nnumhl\25LspDiagnosticsDefaul\ttext\1\0\0\23LspDiagnosticsSign\16sign_define\afn\bvim√\1\0\3\b\0\t\0\30\18\5\0\0009\3\0\0'\6\1\0B\3\3\2\15\0\3\0X\4\1ÄK\0\1\0006\3\2\0009\3\3\0039\3\4\0039\3\5\3\5\1\3\0X\3\6Ä6\3\2\0009\3\6\0039\3\a\3\18\5\0\0B\3\2\1X\3\nÄ6\3\2\0009\3\6\0039\3\b\0034\5\3\0004\6\3\0>\0\1\6>\6\1\5+\6\2\0004\a\0\0B\3\4\1K\0\1\0\14nvim_echo\21nvim_err_writeln\bapi\nERROR\vlevels\blog\bvim\14exit code\nmatchÛ\t\1\0\14\0008\0â\0016\0\0\0006\2\1\0'\3\2\0B\0\3\0036\2\0\0006\4\1\0'\5\3\0B\2\3\3\14\0\0\0X\4\3Ä\14\0\2\0X\4\1Ä2\0{Ä3\4\4\0006\5\5\0009\5\6\0059\5\a\0059\5\b\5B\5\1\0029\6\t\0059\6\n\0069\6\v\0065\a\r\0=\a\f\0069\6\t\0059\6\n\0069\6\v\6+\a\2\0=\a\14\0069\6\t\0059\6\n\0069\6\v\6+\a\2\0=\a\15\0069\6\t\0059\6\n\0069\6\v\6+\a\2\0=\a\16\0069\6\t\0059\6\n\0069\6\v\6+\a\2\0=\a\17\0069\6\t\0059\6\n\0069\6\v\6+\a\2\0=\a\18\0069\6\t\0059\6\n\0069\6\v\6+\a\2\0=\a\19\0069\6\t\0059\6\n\0069\6\v\0065\a\22\0005\b\21\0=\b\23\a=\a\20\0069\6\t\0059\6\n\0069\6\v\0065\a\26\0005\b\25\0=\b\27\a=\a\24\0063\6\28\0\18\a\6\0B\a\1\0013\a\30\0=\a\29\0033\a\31\0\18\b\a\0'\n \0'\v!\0B\b\3\1\18\b\a\0'\n\"\0'\v#\0B\b\3\1\18\b\a\0'\n$\0'\v%\0B\b\3\1\18\b\a\0'\n&\0'\v'\0B\b\3\0016\b\5\0009\b\6\b9\b(\b6\t\5\0009\t\6\t9\t*\t6\v\5\0009\v\6\v9\v+\v9\v,\v5\f.\0005\r-\0=\r/\fB\t\3\2=\t)\b6\b\5\0009\b\6\b9\b(\b6\t\5\0009\t\6\t9\t*\t6\v\5\0009\v\6\v9\v(\v9\v1\v5\f2\0B\t\3\2=\t0\b6\b\5\0009\b\6\b9\b(\b6\t\5\0009\t\6\t9\t*\t6\v\5\0009\v\6\v9\v(\v9\v4\v5\f5\0B\t\3\2=\t3\b6\b\5\0003\t7\0=\t6\b2\0\0ÄK\0\1\0K\0\1\0\0\vnotify\1\0\1\vborder\vsingle\19signature_help\31textDocument/signatureHelp\1\0\1\vborder\vsingle\nhover\23textDocument/hover\17virtual_text\1\0\3\14underline\2\nsigns\2\21update_in_insert\1\1\0\2\vprefix\bÔëÖ\fspacing\3\0\27on_publish_diagnostics\15diagnostic\twith$textDocument/publishDiagnostics\rhandlers\bÔÅ±\fWarning\bÔ†¥\tHint\bÔüª\16Information\bÔôò\nError\0\0\22post_install_hook\0\15properties\1\0\0\1\4\0\0\18documentation\vdetail\24additionalTextEdits\19resolveSupport\rvalueSet\1\0\0\1\2\0\0\3\1\15tagSupport\28commitCharactersSupport\22deprecatedSupport\24labelDetailsSupport\25insertReplaceSupport\21preselectSupport\19snippetSupport\1\3\0\0\rmarkdown\14plaintext\24documentationFormat\19completionItem\15completion\17textDocument\29make_client_capabilities\rprotocol\blsp\bvim\0\15lspinstall\14lspconfig\frequire\npcall\0", "config", "nvim-lspconfig")

vim.cmd [[ packadd lsp_signature.nvim ]]

-- Config for: lsp_signature.nvim
try_loadstring("\27LJ\2\nó\2\0\0\6\0\a\0\f6\0\0\0006\2\1\0'\3\2\0B\0\3\3\15\0\0\0X\2\5Ä9\2\3\0015\4\4\0005\5\5\0=\5\6\4B\2\2\1K\0\1\0\17handler_opts\1\0\1\vborder\vsingle\1\0\f\14max_width\3x\15max_height\3\22\vzindex\3»\1\17hi_parameter\vSearch\fpadding\5\16hint_scheme\vString\16hint_prefix\tÔüª \16hint_enable\2\ffix_pos\2\20floating_window\2\14doc_lines\3\2\tbind\2\nsetup\18lsp_signature\frequire\npcall\0", "config", "lsp_signature.nvim")

time([[Sequenced loading]], false)

-- Command lazy-loads
time([[Defining lazy-load commands]], true)
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopWord lua require("packer.load")({'hop'}, { cmd = "HopWord", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopLine lua require("packer.load")({'hop'}, { cmd = "HopLine", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopChar1 lua require("packer.load")({'hop'}, { cmd = "HopChar1", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopChar2 lua require("packer.load")({'hop'}, { cmd = "HopChar2", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopPattern lua require("packer.load")({'hop'}, { cmd = "HopPattern", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TZAtaraxis lua require("packer.load")({'TrueZen.nvim'}, { cmd = "TZAtaraxis", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TZMinimalist lua require("packer.load")({'TrueZen.nvim'}, { cmd = "TZMinimalist", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TZFocus lua require("packer.load")({'TrueZen.nvim'}, { cmd = "TZFocus", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file NvimTreeFocus lua require("packer.load")({'nvim-tree.lua'}, { cmd = "NvimTreeFocus", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file StartupTime lua require("packer.load")({'startuptime.vim'}, { cmd = "StartupTime", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file NvimTreeToggle lua require("packer.load")({'nvim-tree.lua'}, { cmd = "NvimTreeToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Telescope lua require("packer.load")({'telescope.nvim'}, { cmd = "Telescope", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Twilight lua require("packer.load")({'twilight.nvim'}, { cmd = "Twilight", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TwilightEnable lua require("packer.load")({'twilight.nvim'}, { cmd = "TwilightEnable", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
time([[Defining lazy-load commands]], false)

vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time([[Defining lazy-load filetype autocommands]], true)
vim.cmd [[au FileType org ++once lua require("packer.load")({'orgmode.nvim'}, { ft = "org" }, _G.packer_plugins)]]
time([[Defining lazy-load filetype autocommands]], false)
  -- Event lazy-loads
time([[Defining lazy-load event autocommands]], true)
vim.cmd [[au InsertEnter * ++once lua require("packer.load")({'friendly-snippets'}, { event = "InsertEnter *" }, _G.packer_plugins)]]
vim.cmd [[au BufRead * ++once lua require("packer.load")({'nvim-colorizer.lua', 'nvim-treesitter', 'indent-blankline.nvim'}, { event = "BufRead *" }, _G.packer_plugins)]]
time([[Defining lazy-load event autocommands]], false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time([[Sourcing ftdetect script at: /home/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org.vim]], true)
vim.cmd [[source /home/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org.vim]]
time([[Sourcing ftdetect script at: /home/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org.vim]], false)
time([[Sourcing ftdetect script at: /home/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org_archive.vim]], true)
vim.cmd [[source /home/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org_archive.vim]]
time([[Sourcing ftdetect script at: /home/shauryasingh/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org_archive.vim]], false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
