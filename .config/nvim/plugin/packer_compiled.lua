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
local package_path_str = "/home/shaunsingh0207/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?.lua;/home/shaunsingh0207/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?/init.lua;/home/shaunsingh0207/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?.lua;/home/shaunsingh0207/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/shaunsingh0207/.cache/nvim/packer_hererocks/2.0.5/lib/lua/5.1/?.so"
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
    config = { "\27LJ\1\2∆\1\0\0\4\0\b\0\0174\0\0\0004\1\1\0%\2\2\0>\0\3\3\14\0\0\0T\2\1ÄG\0\1\0007\2\3\0017\2\4\0023\3\5\0>\2\2\0014\2\1\0%\3\6\0>\2\2\0027\2\a\2>\2\1\1G\0\1\0\tload luasnip/loaders/from_vscode\1\0\2\fhistory\2\17updateevents\29TextChanged,TextChangedI\15set_config\vconfig\fluasnip\frequire\npcall\0" },
    load_after = {
      ["nvim-cmp"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/LuaSnip",
    wants = { "friendly-snippets" }
  },
  ["TrueZen.nvim"] = {
    commands = { "TZAtaraxis", "TZMinimalist", "TZFocus" },
    config = { "\27LJ\1\2º\4\0\0\a\0\23\0\0294\0\0\0004\1\1\0%\2\2\0>\0\3\3\14\0\0\0T\2\1ÄG\0\1\0007\2\3\0013\3\v\0003\4\5\0003\5\4\0:\5\6\0043\5\a\0:\5\b\0043\5\t\0:\5\n\4:\4\f\0033\4\16\0003\5\r\0003\6\14\0:\6\15\5:\5\17\0043\5\18\0:\5\19\4:\4\20\0033\4\21\0:\4\22\3>\2\2\1G\0\1\0\17integrations\1\0\3\15galaxyline\2\rtwilight\2\20nvim_bufferline\2\nmodes\nfocus\1\0\2\17focus_method\17experimental\20margin_of_error\3\5\rataraxis\1\0\0\29ideal_writing_area_width\1\2\0\0\3\0\1\0\a\17left_padding\3 \18right_padding\3 \21bg_configuration\2\19bottom_padding\3\1 keep_default_fold_fillchars\1\17auto_padding\1\16top_padding\3\1\aui\1\0\0\tleft\1\0\3\15signcolumn\ano\19relativenumber\1\vnumber\1\btop\1\0\1\16showtabline\3\0\vbottom\1\0\0\1\0\5\rshowmode\2\fshowcmd\1\15laststatus\3\0\nruler\2\14cmdheight\3\1\nsetup\rtrue-zen\frequire\npcall\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/TrueZen.nvim"
  },
  ["alpha-nvim"] = {
    config = { "\27LJ\1\2˛\19\0\0\b\0\14\1\0244\0\0\0%\1\1\0>\0\2\0024\1\0\0%\2\2\0>\1\2\0027\2\3\0017\2\4\0023\3\6\0:\3\5\0027\2\3\0017\2\a\0022\3\3\0007\4\b\1%\5\t\0%\6\n\0%\a\v\0>\4\4\0<\4\0\0:\3\5\0027\2\f\0007\3\r\1>\2\2\1G\0\1\0\topts\nsetup\f:qa<CR>\bÔåå\tv0.9\vbutton\fbuttons\1#\0\0I            :h-                                  Nhy`               I           -mh.                           h.    `Ndho               I           hmh+                          oNm.   oNdhh               I          `Nmhd`                        /NNmd  /NNhhd               I          -NNhhy                      `hMNmmm`+NNdhhh               I          .NNmhhs              ```....`..-:/./mNdhhh+               I           mNNdhhh-     `.-::///+++////++//:--.`-/sd`               I           oNNNdhhdo..://++//++++++/+++//++///++/-.`                I      y.   `mNNNmhhhdy+/++++//+/////++//+++///++////-` `/oos:       I .    Nmy:  :NNNNmhhhhdy+/++/+++///:.....--:////+++///:.`:s+        I h-   dNmNmy oNNNNNdhhhhy:/+/+++/-         ---:/+++//++//.`         I hd+` -NNNy`./dNNNNNhhhh+-://///    -+oo:`  ::-:+////++///:`        I /Nmhs+oss-:++/dNNNmhho:--::///    /mmmmmo  ../-///++///////.       I  oNNdhhhhhhhs//osso/:---:::///    /yyyyso  ..o+-//////////:/.      I   /mNNNmdhhhh/://+///::://////     -:::- ..+sy+:////////::/:/.     I     /hNNNdhhs--:/+++////++/////.      ..-/yhhs-/////////::/::/`    I       .ooo+/-::::/+///////++++//-/ossyyhhhhs/:///////:::/::::/:    I       -///:::::::////++///+++/////:/+ooo+/::///////.::://::---+`   I       /////+//++++/////+////-..//////////::-:::--`.:///:---:::/:   I       //+++//++++++////+++///::--                 .::::-------::   I       :/++++///////////++++//////.                -:/:----::../-   I       -/++++//++///+//////////////               .::::---:::-.+`   I       `////////////////////////////:.            --::-----...-/    I        -///://////////////////////::::-..      :-:-:-..-::.`.+`    I         :/://///:///::://::://::::::/:::::::-:---::-.-....``/- -   I           ::::://::://::::::::::::::----------..-:....`.../- -+oo/ I            -/:::-:::::---://:-::-::::----::---.-.......`-/.      ``I           s-`::--:::------:////----:---.-:::...-.....`./:          I          yMNy.`::-.--::..-dmmhhhs-..-.-.......`.....-/:`           I         oMNNNh. `-::--...:NNNdhhh/.--.`..``.......:/-              I        :dy+:`      .-::-..NNNhhd+``..`...````.-::-`                I                        .-:mNdhh:.......--::::-`                    I                           yNh/..------..`                          I                                                                    \bval\vheader\fsection\27alpha.themes.dashboard\nalpha\frequire\3ÄÄ¿ô\4\0" },
    loaded = true,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/start/alpha-nvim"
  },
  ["bufferline.nvim"] = {
    config = { "\27LJ\1\2D\0\0\3\1\4\0\0064\0\0\0007\0\1\0007\0\2\0+\1\0\0%\2\3\0@\0\3\0\0¿\14term_type\21nvim_buf_get_var\bapi\bvimÄ\1\1\1\4\0\4\0\0264\1\0\0001\2\1\0>\1\2\3\15\0\1\0T\3\16Ä\a\2\2\0T\3\4Ä)\3\1\0000\0\0ÄH\3\2\0T\3\rÄ\a\2\3\0T\3\4Ä)\3\1\0000\0\0ÄH\3\2\0T\3\aÄ)\3\2\0000\0\0ÄH\3\2\0T\3\3Ä)\3\2\0000\0\0ÄH\3\2\0000\0\0ÄG\0\1\0\thori\tvert\0\npcall¯\f\1\0\b\0@\0ç\0014\0\0\0004\1\1\0%\2\2\0>\0\3\3\14\0\0\0T\2\1Ä0\0ÖÄ4\2\3\0007\2\4\2%\3\5\0>\2\2\0013\2\6\0007\3\a\0013\4\r\0003\5\t\0002\6\3\0003\a\b\0;\a\1\6:\6\n\0051\6\v\0:\6\f\5:\5\14\0043\5\20\0003\6\16\0007\a\15\2:\a\17\0067\a\18\2:\a\19\6:\6\21\0053\6\23\0007\a\22\2:\a\17\0067\a\24\2:\a\19\6:\6\25\0053\6\27\0007\a\26\2:\a\17\0067\a\18\2:\a\19\6:\6\28\0053\6\29\0007\a\26\2:\a\17\0067\a\18\2:\a\19\6:\6\30\0053\6\31\0007\a\26\2:\a\17\0067\a\18\2:\a\19\6:\6 \0053\6!\0007\a\26\2:\a\17\0067\a\18\2:\a\19\6:\6\"\0053\6#\0007\a\26\2:\a\17\0067\a\18\2:\a\19\6:\6$\0053\6&\0007\a%\2:\a\17\0067\a\24\2:\a\19\6:\6'\0053\6(\0007\a\15\2:\a\17\0067\a\18\2:\a\19\6:\6)\0053\6*\0007\a\24\2:\a\17\0067\a\24\2:\a\19\6:\6+\0053\6,\0007\a%\2:\a\17\0067\a\18\2:\a\19\6:\6-\0053\6.\0007\a%\2:\a\17\0067\a\18\2:\a\19\6:\6/\0053\0061\0007\a0\2:\a\17\0067\a\24\2:\a\19\6:\0062\0053\0063\0007\a\18\2:\a\17\0067\a\18\2:\a\19\6:\0064\0053\0065\0007\a\18\2:\a\17\0067\a\18\2:\a\19\6:\0066\0053\0067\0007\a\18\2:\a\17\0067\a\18\2:\a\19\6:\0068\0053\0069\0007\a\15\2:\a\17\0067\a\26\2:\a\19\6:\6:\0053\6;\0007\a\18\2:\a\17\0067\a\15\2:\a\19\6:\6<\0053\6=\0007\a%\2:\a\17\0067\a\24\2:\a\19\6:\6>\5:\5?\4>\3\2\1G\0\1\0G\0\1\0\15highlights\14tab_close\1\0\0\17tab_selected\1\0\0\btab\1\0\0\23separator_selected\1\0\0\22separator_visible\1\0\0\14separator\1\0\0\22modified_selected\1\0\0\ngreen\21modified_visible\1\0\0\rmodified\1\0\0\23indicator_selected\1\0\0\tfill\1\0\0\26close_button_selected\1\0\0\bred\25close_button_visible\1\0\0\17close_button\1\0\0\21error_diagnostic\1\0\0\nerror\1\0\0\19buffer_visible\1\0\0\tgray\20buffer_selected\nblack\1\0\1\bgui\tbold\nwhite\15background\1\0\0\nguibg\vblack2\nguifg\1\0\0\afg\foptions\1\0\0\18custom_filter\0\foffsets\1\0\16\25enforce_regular_tabs\1\22left_trunc_marker\bÔÇ®\rtab_size\3\20\16diagnostics\1\24show_tab_indicators\2\28show_buffer_close_icons\2\20separator_style\tthin\22buffer_close_icon\bÔôï\20max_name_length\3\14\15close_icon\bÔåå\tview\16multiwindow\23right_trunc_marker\bÔÇ©\22max_prefix_length\3\r\27always_show_bufferline\1\20show_close_icon\2\18modified_icon\bÔëÑ\1\0\3\ttext\5\fpadding\3\1\rfiletype\rNvimTree\nsetup\1\0\15\tblue\f#5E81AC\nblack\f#3b4252\abg\tNONE\vyellow\f#EBCB8B\rdarkblue\f#81A1C1\vblack2\f#2E3440\ngreen\f#8FBCBB\fmagenta\f#BF616A\vorange\f#D08770\tcyan\f#A3BE8C\afg\f#E5E9F0\tgray\f#616E88\bred\f#BF616A\vpurple\f#B48EAD\nwhite\f#ECEFF4Bfunction! Doom_bufferline_quitvim(a,b,c,d) \n qa \n endfunction\bcmd\bvim\15bufferline\frequire\npcall\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/bufferline.nvim"
  },
  ["cmp-buffer"] = {
    after_files = { "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/cmp-buffer/after/plugin/cmp_buffer.lua" },
    load_after = {
      ["cmp-nvim-lsp"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/cmp-buffer"
  },
  ["cmp-nvim-lsp"] = {
    after = { "cmp-buffer" },
    after_files = { "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lsp/after/plugin/cmp_nvim_lsp.lua" },
    load_after = {
      ["cmp-nvim-lua"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lsp"
  },
  ["cmp-nvim-lua"] = {
    after = { "cmp-nvim-lsp" },
    after_files = { "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lua/after/plugin/cmp_nvim_lua.lua" },
    load_after = {
      cmp_luasnip = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lua"
  },
  cmp_luasnip = {
    after = { "cmp-nvim-lua" },
    after_files = { "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/cmp_luasnip/after/plugin/cmp_luasnip.lua" },
    load_after = {
      LuaSnip = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/cmp_luasnip"
  },
  ["friendly-snippets"] = {
    after = { "nvim-cmp" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/friendly-snippets"
  },
  ["galaxyline.nvim"] = {
    config = { "\27LJ\1\2ƒ\1\0\0\3\1\16\0 3\0\1\0+\1\0\0007\1\0\1:\1\2\0+\1\0\0007\1\3\1:\1\4\0+\1\0\0007\1\5\1:\1\6\0+\1\0\0007\1\a\1:\1\b\0+\1\0\0007\1\a\1:\1\t\0+\1\0\0007\1\a\1:\1\n\0+\1\0\0007\1\v\1:\1\f\0004\1\r\0007\1\14\0017\1\15\1>\1\1\0026\1\1\0\v\1\0\0T\2\2Ä+\2\0\0007\1\v\2H\1\2\0\3¿\tmode\afn\bvim\6R\bred\6v\6\22\6V\fmagenta\6c\vorange\6i\ngreen\6n\1\0\0\tcyan˘\1\0\0\5\1\t\0\0263\0\0\0004\1\1\0007\1\2\0017\1\3\1%\2\4\0+\3\0\0>\3\1\2$\2\3\2>\1\2\0014\1\1\0007\1\5\0017\1\6\1>\1\1\0026\1\1\0\v\1\0\0T\2\5Ä4\2\1\0007\2\5\0027\2\6\2>\2\1\2\16\1\2\0%\2\a\0\16\3\1\0%\4\b\0$\2\4\2H\2\2\0\4¿\6 \a  \tmode\afn\27hi GalaxyViMode guibg=\17nvim_command\bapi\bvim\1\0\a\6\22\rDOOM Ô¶à\6c\rDOOM Ôíâ\6V\rDOOM Ô¶à\6R\rDOOM Ôê¢\6n\fDOOM Œª\6v\rDOOM Ô¶à\6i\rDOOM ÓüÖ\21\0\0\1\0\1\0\2%\0\0\0H\0\2\0\n Ôû° ∞\1\0\0\6\0\b\0\0254\0\0\0%\1\1\0>\0\2\0027\1\2\0>\1\1\0024\2\3\0007\2\4\2\16\3\1\0>\2\2\2'\3\28\0\1\3\2\0T\2\tÄ4\2\3\0007\2\5\2\16\3\1\0'\4\1\0'\5\25\0>\2\4\2%\3\6\0$\2\3\2H\2\2\0\16\2\1\0%\3\a\0$\2\3\2H\2\2\0\6 \b...\bsub\blen\vstring\19get_git_branch\29galaxyline.providers.vcs\frequire\20\0\0\1\0\1\0\2%\0\0\0H\0\2\0\tÓÇ∫ \17\0\0\1\0\1\0\2%\0\0\0H\0\2\0\6 \17\0\0\1\0\1\0\2%\0\0\0H\0\2\0\6 '\0\0\1\0\3\0\0044\0\0\0007\0\1\0007\0\2\0H\0\2\0\rfiletype\abo\bvim\21\0\0\1\0\1\0\2%\0\0\0H\0\2\0\n Ôåå ﬂ\19\1\0\v\0`\0∂\0024\0\0\0%\1\1\0>\0\2\0024\1\0\0%\2\2\0>\1\2\0027\2\3\0003\3\5\0:\3\4\0003\3\6\0001\4\a\0007\5\b\0023\6\16\0003\a\n\0001\b\t\0:\b\v\a2\b\3\0007\t\f\3;\t\1\b7\t\f\3;\t\2\b:\b\r\a2\b\3\0007\t\14\3;\t\1\b7\t\14\3;\t\2\b:\b\15\a:\a\17\6;\6\1\0057\5\b\0023\6\21\0003\a\18\0002\b\3\0004\t\0\0%\n\19\0>\t\2\0027\t\20\t;\t\1\b7\t\14\3;\t\2\b:\b\r\a:\a\22\6;\6\2\0057\5\b\0023\6\26\0003\a\23\0002\b\3\0007\t\24\3;\t\1\b7\t\14\3;\t\2\b:\b\r\a2\b\3\0007\t\14\3;\t\1\b7\t\25\3;\t\2\b:\b\15\a:\a\27\6;\6\3\0057\5\b\0023\6!\0003\a\29\0001\b\28\0:\b\v\a7\b\30\1:\b\31\a2\b\3\0007\t \3;\t\1\b7\t\25\3;\t\2\b:\b\r\a:\a\"\6;\6\4\0057\5\b\0023\6%\0003\a$\0001\b#\0:\b\v\a7\b\30\1:\b\31\a2\b\3\0007\t\24\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a:\a&\6;\6\5\0057\5\b\0023\6)\0003\a'\0007\b\30\1:\b\31\a2\b\3\0007\t(\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a:\a*\6;\6\6\0057\5\b\0023\6-\0003\a+\0007\b\30\1:\b\31\a2\b\3\0007\t,\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a:\a.\6;\6\a\0057\5\b\0023\0060\0003\a/\0007\b\30\1:\b\31\a2\b\3\0007\t \3;\t\1\b7\t\25\3;\t\2\b:\b\r\a:\a1\6;\6\b\0057\5\b\0023\0065\0003\a3\0001\b2\0:\b\v\a2\b\3\0007\t4\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a:\a6\6;\6\t\0057\5\b\0023\0068\0003\a7\0002\b\3\0007\t \3;\t\1\b7\t4\3;\t\2\b:\b\r\a:\a9\6;\6\n\0057\5\b\0023\6<\0003\a;\0001\b:\0:\b\v\a2\b\3\0007\t\25\3;\t\1\b7\t4\3;\t\2\b:\b\r\a:\a=\6;\6\v\0057\5\b\0023\6?\0003\a>\0002\b\3\0007\t,\3;\t\1\b7\t4\3;\t\2\b:\b\r\a:\a@\6;\6\f\0057\5\b\0023\6B\0003\aA\0002\b\3\0007\t\24\3;\t\1\b7\t4\3;\t\2\b:\b\r\a:\aC\6;\6\r\0057\5\b\0023\6F\0003\aE\0001\bD\0:\b\v\a2\b\3\0007\t4\3;\t\1\b7\t4\3;\t\2\b:\b\r\a:\a=\6;\6\14\0057\5\b\0023\6I\0003\aG\0002\b\3\0007\tH\3;\t\1\b7\t4\3;\t\2\b:\b\r\a2\b\3\0007\t4\3;\t\1\b7\t\f\3;\t\2\b:\b\15\a:\aJ\6;\6\15\0057\5K\0023\6N\0003\aM\0001\bL\0:\b\v\a2\b\3\0007\t\24\3;\t\1\b7\t4\3;\t\2\b:\b\r\a2\b\3\0007\t4\3;\t\1\b7\t\f\3;\t\2\b:\b\15\a:\aO\6;\6\1\0057\5K\0023\6Q\0003\aP\0002\b\3\0007\t\24\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a2\b\3\0007\t\25\3;\t\1\b7\t4\3;\t\2\b:\b\15\a:\aR\6;\6\2\0057\5K\0023\6U\0003\aT\0001\bS\0:\b\v\a2\b\3\0007\t \3;\t\1\b7\t\14\3;\t\2\b:\b\r\a2\b\3\0007\t\14\3;\t\1\b7\t\25\3;\t\2\b:\b\15\a:\aV\6;\6\3\0057\5W\0023\6Y\0003\aX\0002\b\3\0007\t\24\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a2\b\3\0007\t\25\3;\t\1\b7\t\f\3;\t\2\b:\b\15\a:\aZ\6;\6\1\0057\5[\0023\6^\0003\a\\\0002\b\3\0007\t]\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a2\b\3\0007\t\25\3;\t\1\b7\t\f\3;\t\2\b:\b\15\a:\a_\6;\6\1\0050\0\0ÄG\0\1\0\15BufferIcon\1\0\0\vyellow\1\0\2\14separator\tÓÇ∫ \rprovider\15BufferIcon\21short_line_right\15BufferType\1\0\0\1\0\2\14separator\tÓÇ∏ \rprovider\17FileTypeName\20short_line_left\tLogo\1\0\0\1\0\1\14separator\n ÓÇ∫ \0\rLineInfo\1\0\0\1\0\2\14separator\n ÓÇ∫ \rprovider\15LineColumn\15FileFormat\1\0\0\1\0\1\14separator\tÓÇ∫ \0\nright\19DiagnosticInfo\1\0\0\tblue\1\0\3\14separator\tÓÇ∏ \rprovider\19DiagnosticInfo\ticon\n ÔÅö \1\0\0\1\0\0\0\19DiagnosticHint\1\0\0\1\0\2\ticon\n ÓÄä \rprovider\19DiagnosticHint\19DiagnosticWarn\1\0\0\1\0\2\ticon\n ÔÅ± \rprovider\19DiagnosticWarn\nSpace\1\0\0\1\0\0\0\20DiagnosticError\1\0\0\1\0\2\ticon\n ÔÅó \rprovider\20DiagnosticError\fLeftEnd\1\0\0\vfilebg\1\0\0\0\15DiffRemove\1\0\0\1\0\2\ticon\tÔëò \rprovider\15DiffRemove\17DiffModified\1\0\0\vorange\1\0\2\ticon\tÔëô \rprovider\17DiffModified\fDiffAdd\1\0\0\ngreen\1\0\2\ticon\tÔëó \rprovider\fDiffAdd\14GitBranch\1\0\0\1\0\0\0\fGitIcon\1\0\0\bred\14condition\24check_git_workspace\1\0\0\0\rFileName\1\0\0\15section_bg\afg\1\0\2\14separator\tÓÇ∏ \rprovider\rFileName\rFileIcon\1\0\0\24get_file_icon_color\"galaxyline.providers.fileinfo\1\0\1\rprovider\rFileIcon\vViMode\1\0\0\24separator_highlight\16section_bg2\14highlight\abg\rprovider\1\0\1\14separator\6 \0\tleft\0\1\0\r\tblue\f#5E81AC\ngreen\f#A3BE8C\fmagenta\f#BF616A\vyellow\f#EBCB8B\rdarkblue\f#81A1C1\abg\f#3B4252\tcyan\f#8FBCBB\vorange\f#D08770\afg\f#E5E9F0\16section_bg2\f#4C566A\bred\f#BF616A\vfilebg\f#3B4252\15section_bg\f#434C5E\1\3\0\0\rNvimTree\vpacker\20short_line_list\fsection\25galaxyline.condition\15galaxyline\frequire\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/galaxyline.nvim"
  },
  ["gitsigns.nvim"] = {
    config = { "\27LJ\1\2ˆ\f\0\0\4\0\26\0\0294\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\14\0003\2\4\0003\3\3\0:\3\5\0023\3\6\0:\3\a\0023\3\b\0:\3\t\0023\3\n\0:\3\v\0023\3\f\0:\3\r\2:\2\15\0013\2\16\0003\3\17\0:\3\18\0023\3\19\0:\3\20\2:\2\21\0013\2\22\0:\2\23\0013\2\24\0:\2\25\1>\0\2\1G\0\1\0\tyadm\1\0\1\venable\2\16watch_index\1\0\2\rinterval\3Ë\a\17follow_files\2\fkeymaps\tn [c\1\2\1\0H&diff ? '[c' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'\texpr\2\tn ]c\1\2\1\0H&diff ? ']c' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'\texpr\2\1\0\r\fnoremap\2\17v <leader>hrT<cmd>lua require\"gitsigns\".reset_hunk({vim.fn.line(\".\"), vim.fn.line(\"v\")})<CR>\17n <leader>hR2<cmd>lua require\"gitsigns\".reset_buffer()<CR>\17n <leader>hs0<cmd>lua require\"gitsigns\".stage_hunk()<CR>\17n <leader>hb4<cmd>lua require\"gitsigns\".blame_line(true)<CR>\to ih::<C-U>lua require\"gitsigns.actions\".select_hunk()<CR>\tx ih::<C-U>lua require\"gitsigns.actions\".select_hunk()<CR>\17n <leader>hr0<cmd>lua require\"gitsigns\".reset_hunk()<CR>\17n <leader>hU8<cmd>lua require\"gitsigns\".reset_buffer_index()<CR>\17v <leader>hsT<cmd>lua require\"gitsigns\".stage_hunk({vim.fn.line(\".\"), vim.fn.line(\"v\")})<CR>\17n <leader>hp2<cmd>lua require\"gitsigns\".preview_hunk()<CR>\17n <leader>hS2<cmd>lua require\"gitsigns\".stage_buffer()<CR>\17n <leader>hu5<cmd>lua require\"gitsigns\".undo_stage_hunk()<CR>\nsigns\1\0\5\20update_debounce\3»\1\vlinehl\1\nnumhl\1\15signcolumn\2\14word_diff\1\17changedelete\1\0\4\ttext\6~\nnumhl\21GitSignsChangeNr\vlinehl\21GitSignsChangeLn\ahl\19GitSignsChange\14topdelete\1\0\4\ttext\b‚Äæ\nnumhl\21GitSignsDeleteNr\vlinehl\21GitSignsDeleteLn\ahl\19GitSignsDelete\vdelete\1\0\4\ttext\6_\nnumhl\21GitSignsDeleteNr\vlinehl\21GitSignsDeleteLn\ahl\19GitSignsDelete\vchange\1\0\4\ttext\b‚îÇ\nnumhl\21GitSignsChangeNr\vlinehl\21GitSignsChangeLn\ahl\19GitSignsChange\badd\1\0\0\1\0\4\ttext\b‚îÇ\nnumhl\18GitSignsAddNr\vlinehl\18GitSignsAddLn\ahl\16GitSignsAdd\nsetup\rgitsigns\frequire\0" },
    loaded = true,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/start/gitsigns.nvim"
  },
  hop = {
    commands = { "HopWord", "HopLine", "HopChar1", "HopChar2", "HopPattern" },
    config = { "\27LJ\1\0021\0\0\2\0\3\0\0064\0\0\0%\1\1\0>\0\2\0027\0\2\0>\0\1\1G\0\1\0\nsetup\bhop\frequire\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/hop"
  },
  ["indent-blankline.nvim"] = {
    config = { "\27LJ\1\2å\4\0\0\3\0\n\0\r4\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\3\0003\2\4\0:\2\5\0013\2\6\0:\2\a\0013\2\b\0:\2\t\1>\0\2\1G\0\1\0\20buftype_exclude\1\2\0\0\rterminal\21filetype_exclude\1\b\0\0\thelp\rterminal\14dashboard\vpacker\flspinfo\20TelescopePrompt\21TelescopeResults\21context_patterns\1\21\0\0\nclass\vreturn\rfunction\vmethod\b^if\v^while\16jsx_element\t^for\f^object\v^table\nblock\14arguments\17if_statement\16else_clause\16jsx_element\29jsx_self_closing_element\18try_statement\17catch_clause\21import_statement\19operation_type\1\0\3\25show_current_context\2\28show_first_indent_level\1#show_trailing_blankline_indent\1\nsetup\21indent_blankline\frequire\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/indent-blankline.nvim"
  },
  ["lsp_signature.nvim"] = {
    config = { "\27LJ\1\2ó\2\0\0\5\0\a\0\f4\0\0\0004\1\1\0%\2\2\0>\0\3\3\15\0\0\0T\2\5Ä7\2\3\0013\3\4\0003\4\5\0:\4\6\3>\2\2\1G\0\1\0\17handler_opts\1\0\1\vborder\vsingle\1\0\f\15max_height\3\22\14doc_lines\3\2\20floating_window\2\tbind\2\16hint_prefix\tÔüª \ffix_pos\2\vzindex\3»\1\17hi_parameter\vSearch\16hint_scheme\vString\16hint_enable\2\14max_width\3x\fpadding\5\nsetup\18lsp_signature\frequire\npcall\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/lsp_signature.nvim"
  },
  ["nord.nvim"] = {
    after = { "nvim-web-devicons" },
    config = { "\27LJ\1\2≈\1\0\0\2\0\b\0\0214\0\0\0007\0\1\0)\1\1\0:\1\2\0004\0\0\0007\0\1\0)\1\1\0:\1\3\0004\0\0\0007\0\1\0)\1\2\0:\1\4\0004\0\0\0007\0\1\0)\1\1\0:\1\5\0004\0\0\0007\0\6\0%\1\a\0>\0\2\1G\0\1\0\21colorscheme nord\bcmd\28nord_disable_background nord_cursorline_transparent\18nord_contrast\17nord_borders\6g\bvim\0" },
    loaded = true,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/nord.nvim"
  },
  ["nvim-autopairs"] = {
    config = { '\27LJ\1\2≠\1\0\0\6\0\6\0\0194\0\0\0004\1\1\0%\2\2\0>\0\3\0034\2\0\0004\3\1\0%\4\3\0>\2\3\3\14\0\0\0T\4\3Ä\14\0\2\0T\4\1ÄG\0\1\0007\4\4\1>\4\1\0017\4\4\0033\5\5\0>\4\2\1G\0\1\0\1\0\2\17map_complete\2\vmap_cr\2\nsetup"nvim-autopairs.completion.cmp\19nvim-autopairs\frequire\npcall\0' },
    load_after = {
      ["nvim-cmp"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/nvim-autopairs"
  },
  ["nvim-cmp"] = {
    after = { "nvim-autopairs", "LuaSnip" },
    config = { "\27LJ\1\2C\0\1\3\0\4\0\a4\1\0\0%\2\1\0>\1\2\0027\1\2\0017\2\3\0>\1\2\1G\0\1\0\tbody\15lsp_expand\fluasnip\frequireë\3\0\2\4\0\6\0\n3\2\1\0007\3\2\0007\3\3\0036\2\3\2:\2\0\0013\2\5\0007\3\4\0016\2\3\2:\2\4\1H\1\2\0\1\0\25\tText\bÔùæ\rFunction\bÔûî\rOperator\bÔöî\nColor\bÔ£ó\nClass\bÔ¥Ø\vModule\bÔíá\rConstant\bÔ£æ\rProperty\bÔ∞†\vMethod\bÔö¶\15EnumMember\bÔÖù\tUnit\bÔ•¨\14Interface\bÔÉ®\vStruct\bÔ≠Ñ\rVariable\bÔî™\fSnippet\bÔëè\vFolder\bÔùä\nEvent\bÔÉß\nField\bÔ∞†\18TypeParameter\5\tFile\bÔúò\nValue\bÔ¢ü\16Constructor\bÔê£\tEnum\bÔÖù\fKeyword\bÔ†ä\14Reference\bÔúÜ\tkind\tname\vsource\1\0\2\rnvim_lsp\f[LSPŒª]\vbuffer\r[BUFÓòí]\tmenu∑\2\0\1\a\0\r\1,4\1\0\0007\1\1\0017\1\2\1>\1\1\2\t\1\0\0T\1\14Ä4\1\0\0007\1\1\0017\1\3\0014\2\0\0007\2\4\0027\2\5\2%\3\6\0)\4\2\0)\5\2\0)\6\2\0>\2\5\2%\3\a\0>\1\3\1T\1\23Ä4\1\b\0%\2\t\0>\1\2\0027\1\n\1>\1\1\2\15\0\1\0T\2\14Ä4\1\0\0007\1\1\0017\1\3\0014\2\0\0007\2\4\0027\2\5\2%\3\v\0)\4\2\0)\5\2\0)\6\2\0>\2\5\2%\3\f\0>\1\3\1T\1\2Ä\16\1\0\0>\1\1\1G\0\1\0\5!<Plug>luasnip-expand-or-jump\23expand_or_jumpable\fluasnip\frequire\6n\n<C-n>\27nvim_replace_termcodes\bapi\rfeedkeys\15pumvisible\afn\bvim\2¨\2\0\1\a\0\r\1-4\1\0\0007\1\1\0017\1\2\1>\1\1\2\t\1\0\0T\1\14Ä4\1\0\0007\1\1\0017\1\3\0014\2\0\0007\2\4\0027\2\5\2%\3\6\0)\4\2\0)\5\2\0)\6\2\0>\2\5\2%\3\a\0>\1\3\1T\1\24Ä4\1\b\0%\2\t\0>\1\2\0027\1\n\1'\2ˇˇ>\1\2\2\15\0\1\0T\2\14Ä4\1\0\0007\1\1\0017\1\3\0014\2\0\0007\2\4\0027\2\5\2%\3\v\0)\4\2\0)\5\2\0)\6\2\0>\2\5\2%\3\f\0>\1\3\1T\1\2Ä\16\1\0\0>\1\1\1G\0\1\0\5\28<Plug>luasnip-jump-prev\rjumpable\fluasnip\frequire\6n\n<C-p>\27nvim_replace_termcodes\bapi\rfeedkeys\15pumvisible\afn\bvim\2≤\5\1\0\b\0.\0L4\0\0\0004\1\1\0%\2\2\0>\0\3\3\14\0\0\0T\2\1Ä0\0DÄ4\2\3\0007\2\4\2%\3\6\0:\3\5\0027\2\a\0013\3\v\0003\4\t\0001\5\b\0:\5\n\4:\4\f\0033\4\14\0001\5\r\0:\5\15\4:\4\16\0033\4\19\0007\5\17\0017\5\18\5>\5\1\2:\5\20\0047\5\17\0017\5\21\5>\5\1\2:\5\22\0047\5\17\0017\5\23\5'\6¸ˇ>\5\2\2:\5\24\0047\5\17\0017\5\23\5'\6\4\0>\5\2\2:\5\25\0047\5\17\0017\5\26\5>\5\1\2:\5\27\0047\5\17\0017\5\28\5>\5\1\2:\5\29\0047\5\17\0017\5\30\0053\6!\0007\a\31\0017\a \a:\a\"\6>\5\2\2:\5#\0041\5$\0:\5%\0041\5&\0:\5'\4:\4\17\0032\4\6\0003\5(\0;\5\1\0043\5)\0;\5\2\0043\5*\0;\5\3\0043\5+\0;\5\4\0043\5,\0;\5\5\4:\4-\3>\2\2\1G\0\1\0G\0\1\0\fsources\1\0\1\tname\forgmode\1\0\1\tname\rnvim_lua\1\0\1\tname\vbuffer\1\0\1\tname\fluasnip\1\0\1\tname\rnvim_lsp\f<S-Tab>\0\n<Tab>\0\t<CR>\rbehavior\1\0\1\vselect\2\fReplace\20ConfirmBehavior\fconfirm\n<C-e>\nclose\14<C-Space>\rcomplete\n<C-f>\n<C-d>\16scroll_docs\n<C-n>\21select_next_item\n<C-p>\1\0\0\21select_prev_item\fmapping\15formatting\vformat\1\0\0\0\fsnippet\1\0\0\vexpand\1\0\0\0\nsetup\21menuone,noselect\16completeopt\bopt\bvim\bcmp\frequire\npcall\0" },
    load_after = {
      ["friendly-snippets"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/nvim-cmp"
  },
  ["nvim-colorizer.lua"] = {
    config = { "\27LJ\1\2◊\1\0\0\5\0\t\0\0154\0\0\0004\1\1\0%\2\2\0>\0\3\3\15\0\0\0T\2\bÄ7\2\3\0013\3\4\0003\4\5\0>\2\3\0014\2\6\0007\2\a\2%\3\b\0>\2\2\1G\0\1\0\30ColorizerReloadAllBuffers\bcmd\bvim\1\0\t\vhsl_fn\1\tmode\15foreground\nnames\1\bcss\1\vRRGGBB\2\vrgb_fn\1\rRRGGBBAA\1\vcss_fn\1\bRGB\2\1\2\0\0\6*\nsetup\14colorizer\frequire\npcall\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/nvim-colorizer.lua"
  },
  ["nvim-lspconfig"] = {
    after = { "lsp_signature.nvim" },
    config = { "\27LJ\1\2A\2\0\3\1\3\0\a4\0\0\0007\0\1\0007\0\2\0+\1\0\0C\2\0\0=\0\1\1G\0\1\0\1¿\24nvim_buf_set_keymap\bapi\bvimA\2\0\3\1\3\0\a4\0\0\0007\0\1\0007\0\2\0+\1\0\0C\2\0\0=\0\1\1G\0\1\0\1¿\24nvim_buf_set_option\bapi\bvimò\v\1\2\n\0*\0u1\2\0\0001\3\1\0\16\4\3\0%\5\2\0%\6\3\0>\4\3\0013\4\4\0\16\5\2\0%\6\5\0%\a\6\0%\b\a\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\b\0%\b\t\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\n\0%\b\v\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\f\0%\b\r\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\14\0%\b\15\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\16\0%\b\17\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\18\0%\b\19\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\20\0%\b\21\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\22\0%\b\23\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\24\0%\b\25\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\26\0%\b\27\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\28\0%\b\29\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\30\0%\b\31\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a \0%\b!\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\"\0%\b#\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a$\0%\b%\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a&\0%\b'\0\16\t\4\0>\5\5\1\16\5\2\0%\6(\0%\a\26\0%\b)\0\16\t\4\0>\5\5\0010\0\0ÄG\0\1\0001<cmd>lua vim.lsp.buf.range_code_action()<CR>\6v*<cmd>lua vim.lsp.buf.formatting()<CR>\r<space>f2<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>\r<space>q0<cmd>lua vim.lsp.diagnostic.goto_next()<CR>\a]d0<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>\a[d<<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>\r<space>e*<cmd>lua vim.lsp.buf.references()<CR>\agr+<cmd>lua vim.lsp.buf.code_action()<CR>\14<space>ca&<cmd>lua vim.lsp.buf.rename()<CR>\14<space>rn/<cmd>lua vim.lsp.buf.type_definition()<CR>\r<space>DJ<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>\14<space>wl7<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>\14<space>wr4<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>\14<space>wa.<cmd>lua vim.lsp.buf.signature_help()<CR>\agk.<cmd>lua vim.lsp.buf.implementation()<CR>\agi%<cmd>lua vim.lsp.buf.hover()<CR>\6K*<cmd>lua vim.lsp.buf.definition()<CR>\agd+<cmd>lua vim.lsp.buf.declaration()<CR>\agD\6n\1\0\2\vsilent\2\fnoremap\2\27v:lua.vim.lsp.omnifunc\romnifunc\0\0–\4\0\0\14\4\29\0D+\0\0\0007\0\0\0>\0\1\1+\0\0\0007\0\1\0>\0\1\0024\1\2\0\16\2\0\0>\1\2\4D\0047Ä\6\5\3\0T\6\fÄ+\6\1\0006\6\5\0067\6\0\0063\a\4\0+\b\2\0:\b\5\a+\b\3\0:\b\6\a3\b\a\0:\b\b\a>\6\2\1T\6)Ä\a\5\3\0T\6'Ä+\6\1\0006\6\5\0067\6\0\0063\a\t\0+\b\2\0:\b\5\a+\b\3\0:\b\6\a3\b\n\0:\b\b\a3\b\26\0003\t\14\0003\n\f\0003\v\v\0:\v\r\n:\n\15\t3\n\21\0002\v\0\b4\f\16\0007\f\17\f7\f\18\f%\r\19\0>\f\2\2)\r\2\0009\r\f\v4\f\16\0007\f\17\f7\f\18\f%\r\20\0>\f\2\2)\r\2\0009\r\f\v:\v\22\n:\n\23\t3\n\24\0:\n\25\t:\t\27\b:\b\28\a>\6\2\1B\4\3\3N\4«G\0\1\0\3¿\1¿\4¿\5¿\rsettings\bLua\1\0\0\14telemetry\1\0\1\venable\1\14workspace\flibrary\1\0\2\15maxPreload\3†ç\6\20preloadFileSize\3êN\28$VIMRUNTIME/lua/vim/lsp\20$VIMRUNTIME/lua\vexpand\afn\bvim\16diagnostics\1\0\0\fglobals\1\0\0\1\2\0\0\bvim\1\0\1\26debounce_text_changes\3Ù\3\1\0\0\nflags\1\0\1\26debounce_text_changes\3Ù\3\17capabilities\14on_attach\1\0\0\blua\npairs\22installed_servers\nsetup5\0\0\2\1\3\0\a+\0\0\0>\0\1\0014\0\0\0007\0\1\0%\1\2\0>\0\2\1G\0\1\0\6¿\fbufdo e\bcmd\bvimà\1\0\2\a\0\b\0\0144\2\0\0007\2\1\0027\2\2\2%\3\3\0\16\4\0\0$\3\4\0033\4\4\0:\1\5\4%\5\6\0\16\6\0\0$\5\6\5:\5\a\4>\2\3\1G\0\1\0\nnumhl\25LspDiagnosticsDefaul\ttext\1\0\0\23LspDiagnosticsSign\16sign_define\afn\bvim√\1\0\3\a\0\t\0\30\16\4\0\0007\3\0\0%\5\1\0>\3\3\2\15\0\3\0T\4\1ÄG\0\1\0004\3\2\0007\3\3\0037\3\4\0037\3\5\3\5\1\3\0T\3\6Ä4\3\2\0007\3\6\0037\3\a\3\16\4\0\0>\3\2\1T\3\nÄ4\3\2\0007\3\6\0037\3\b\0032\4\3\0002\5\3\0;\0\1\5;\5\1\4)\5\2\0002\6\0\0>\3\4\1G\0\1\0\14nvim_echo\21nvim_err_writeln\bapi\nERROR\vlevels\blog\bvim\14exit code\nmatchÛ\t\1\0\r\0008\0â\0014\0\0\0004\1\1\0%\2\2\0>\0\3\0034\2\0\0004\3\1\0%\4\3\0>\2\3\3\14\0\0\0T\4\3Ä\14\0\2\0T\4\1Ä0\0{Ä1\4\4\0004\5\5\0007\5\6\0057\5\a\0057\5\b\5>\5\1\0027\6\t\0057\6\n\0067\6\v\0063\a\r\0:\a\f\0067\6\t\0057\6\n\0067\6\v\6)\a\2\0:\a\14\0067\6\t\0057\6\n\0067\6\v\6)\a\2\0:\a\15\0067\6\t\0057\6\n\0067\6\v\6)\a\2\0:\a\16\0067\6\t\0057\6\n\0067\6\v\6)\a\2\0:\a\17\0067\6\t\0057\6\n\0067\6\v\6)\a\2\0:\a\18\0067\6\t\0057\6\n\0067\6\v\6)\a\2\0:\a\19\0067\6\t\0057\6\n\0067\6\v\0063\a\22\0003\b\21\0:\b\23\a:\a\20\0067\6\t\0057\6\n\0067\6\v\0063\a\26\0003\b\25\0:\b\27\a:\a\24\0061\6\28\0\16\a\6\0>\a\1\0011\a\30\0:\a\29\0031\a\31\0\16\b\a\0%\t \0%\n!\0>\b\3\1\16\b\a\0%\t\"\0%\n#\0>\b\3\1\16\b\a\0%\t$\0%\n%\0>\b\3\1\16\b\a\0%\t&\0%\n'\0>\b\3\0014\b\5\0007\b\6\b7\b(\b4\t\5\0007\t\6\t7\t*\t4\n\5\0007\n\6\n7\n+\n7\n,\n3\v.\0003\f-\0:\f/\v>\t\3\2:\t)\b4\b\5\0007\b\6\b7\b(\b4\t\5\0007\t\6\t7\t*\t4\n\5\0007\n\6\n7\n(\n7\n1\n3\v2\0>\t\3\2:\t0\b4\b\5\0007\b\6\b7\b(\b4\t\5\0007\t\6\t7\t*\t4\n\5\0007\n\6\n7\n(\n7\n4\n3\v5\0>\t\3\2:\t3\b4\b\5\0001\t7\0:\t6\b0\0\0ÄG\0\1\0G\0\1\0\0\vnotify\1\0\1\vborder\vsingle\19signature_help\31textDocument/signatureHelp\1\0\1\vborder\vsingle\nhover\23textDocument/hover\17virtual_text\1\0\3\14underline\2\nsigns\2\21update_in_insert\1\1\0\2\fspacing\3\0\vprefix\bÔëÖ\27on_publish_diagnostics\15diagnostic\twith$textDocument/publishDiagnostics\rhandlers\bÔÅ±\fWarning\bÔ†¥\tHint\bÔüª\16Information\bÔôò\nError\0\0\22post_install_hook\0\15properties\1\0\0\1\4\0\0\18documentation\vdetail\24additionalTextEdits\19resolveSupport\rvalueSet\1\0\0\1\2\0\0\3\1\15tagSupport\28commitCharactersSupport\22deprecatedSupport\24labelDetailsSupport\25insertReplaceSupport\21preselectSupport\19snippetSupport\1\3\0\0\rmarkdown\14plaintext\24documentationFormat\19completionItem\15completion\17textDocument\29make_client_capabilities\rprotocol\blsp\bvim\0\15lspinstall\14lspconfig\frequire\npcall\0" },
    loaded = true,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/nvim-lspconfig"
  },
  ["nvim-lspinstall"] = {
    loaded = true,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/start/nvim-lspinstall"
  },
  ["nvim-tree.lua"] = {
    commands = { "NvimTreeToggle", "NvimTreeFocus" },
    config = { "\27LJ\1\2â\22\0\0\n\0q\0ã\0024\0\0\0004\1\1\0%\2\2\0>\0\3\3\14\0\0\0T\2\1ÄG\0\1\0003\2\3\0007\3\4\0014\4\5\0007\4\6\0044\5\5\0007\5\a\5)\6\2\0:\6\b\5'\5\0\0:\5\t\4'\5\1\0:\5\n\4'\5\0\0:\5\v\0043\5\r\0:\5\f\4'\5\0\0:\5\14\4'\5\1\0:\5\15\4'\5\1\0:\5\16\4'\5\1\0:\5\17\4'\5\1\0:\5\18\4'\5\0\0:\5\19\4'\5\0\0:\5\20\4'\5\0\0:\5\21\4'\5\1\0:\5\22\0043\5\24\0:\5\23\4'\5\0\0:\5\25\0044\5\27\0007\5\28\0053\6\29\0004\a\30\0007\a\31\a%\b \0'\tË\3>\a\3\2;\a\2\6>\5\2\2:\5\26\4%\5\"\0:\5!\4'\5\0\0:\5#\4'\5\1\0:\5$\4'\5\25\0:\5%\4'\5\0\0:\5&\0043\5(\0:\5'\0043\5*\0003\6+\0:\6,\0053\6-\0:\6.\5:\5)\0042\5 \0003\0061\0003\a0\0:\a2\6\16\a\3\0%\b3\0>\a\2\2:\a4\6;\6\1\0053\0066\0003\a5\0:\a2\6\16\a\3\0%\b7\0>\a\2\2:\a4\6;\6\2\0053\0068\0\16\a\3\0%\b9\0>\a\2\2:\a4\6;\6\3\0053\6:\0\16\a\3\0%\b;\0>\a\2\2:\a4\6;\6\4\0053\6<\0\16\a\3\0%\b=\0>\a\2\2:\a4\6;\6\5\0053\6>\0\16\a\3\0%\b?\0>\a\2\2:\a4\6;\6\6\0053\6@\0\16\a\3\0%\bA\0>\a\2\2:\a4\6;\6\a\0053\6B\0\16\a\3\0%\bC\0>\a\2\2:\a4\6;\6\b\0053\6D\0\16\a\3\0%\bE\0>\a\2\2:\a4\6;\6\t\0053\6F\0\16\a\3\0%\bE\0>\a\2\2:\a4\6;\6\n\0053\6G\0\16\a\3\0%\bH\0>\a\2\2:\a4\6;\6\v\0053\6I\0\16\a\3\0%\bJ\0>\a\2\2:\a4\6;\6\f\0053\6K\0\16\a\3\0%\bL\0>\a\2\2:\a4\6;\6\r\0053\6M\0\16\a\3\0%\bN\0>\a\2\2:\a4\6;\6\14\0053\6O\0\16\a\3\0%\bP\0>\a\2\2:\a4\6;\6\15\0053\6Q\0\16\a\3\0%\bR\0>\a\2\2:\a4\6;\6\16\0053\6S\0\16\a\3\0%\bT\0>\a\2\2:\a4\6;\6\17\0053\6U\0\16\a\3\0%\bV\0>\a\2\2:\a4\6;\6\18\0053\6W\0\16\a\3\0%\bX\0>\a\2\2:\a4\6;\6\19\0053\6Y\0\16\a\3\0%\bZ\0>\a\2\2:\a4\6;\6\20\0053\6[\0\16\a\3\0%\b\\\0>\a\2\2:\a4\6;\6\21\0053\6]\0\16\a\3\0%\b^\0>\a\2\2:\a4\6;\6\22\0053\6_\0\16\a\3\0%\b`\0>\a\2\2:\a4\6;\6\23\0053\6a\0\16\a\3\0%\bb\0>\a\2\2:\a4\6;\6\24\0053\6c\0\16\a\3\0%\bd\0>\a\2\2:\a4\6;\6\25\0053\6e\0\16\a\3\0%\bf\0>\a\2\2:\a4\6;\6\26\0053\6g\0\16\a\3\0%\bh\0>\a\2\2:\a4\6;\6\27\0053\6i\0\16\a\3\0%\bj\0>\a\2\2:\a4\6;\6\28\0053\6k\0\16\a\3\0%\bl\0>\a\2\2:\a4\6;\6\29\0053\6m\0\16\a\3\0%\bn\0>\a\2\2:\a4\6;\6\30\0053\6o\0\16\a\3\0%\bp\0>\a\2\2:\a4\6;\6\31\5:\5/\4G\0\1\0\16toggle_help\1\0\1\bkey\ag?\nclose\1\0\1\bkey\6q\vdir_up\1\0\1\bkey\6-\18next_git_item\1\0\1\bkey\a}c\18prev_git_item\1\0\1\bkey\a[c\23copy_absolute_path\1\0\1\bkey\agy\14copy_path\1\0\1\bkey\6Y\14copy_name\1\0\1\bkey\6y\npaste\1\0\1\bkey\6p\tcopy\1\0\1\bkey\6c\bcut\1\0\1\bkey\6x\16full_rename\1\0\1\bkey\t<C->\vrename\1\0\1\bkey\6r\vremove\1\0\1\bkey\6d\vcreate\1\0\1\bkey\6a\frefresh\1\0\1\bkey\6R\20toggle_dotfiles\1\0\1\bkey\6H\19toggle_ignored\1\0\1\bkey\6I\17last_sibling\1\0\1\bkey\6J\18first_sibling\1\0\1\bkey\6K\fpreview\1\0\1\bkey\n<Tab>\1\0\1\bkey\v<S-CR>\15close_node\1\0\1\bkey\t<BS>\16parent_node\1\0\1\bkey\6P\17next_sibling\1\0\1\bkey\6>\17prev_sibling\1\0\1\bkey\6<\vtabnew\1\0\1\bkey\n<C-t>\nsplit\1\0\1\bkey\n<C-x>\vvsplit\1\0\1\bkey\n<C-v>\acd\1\0\0\1\3\0\0\19<2-RightMouse>\n<C-]>\acb\tedit\bkey\1\0\0\1\4\0\0\t<CR>\6o\18<2-LeftMouse>\23nvim_tree_bindings\vfolder\1\0\6\topen\bÔêì\15empty_open\bÔêî\fdefault\bÔêì\nempty\bÔêì\fsymlink\bÔíÇ\17symlink_open\bÓóæ\bgit\1\0\a\runstaged\b‚úó\fdeleted\bÔëò\14untracked\b‚òÖ\frenamed\b‚ûú\runmerged\bÓúß\fignored\b‚óå\vstaged\b‚úì\1\0\2\fdefault\bÓòí\fsymlink\bÔíÅ\20nvim_tree_icons\1\0\3\ffolders\3\1\nfiles\3\1\bgit\3\1\25nvim_tree_show_icons\30nvim_tree_lsp_diagnostics\20nvim_tree_width\25nvim_tree_update_cwd\23nvim_tree_tab_open\tleft\19nvim_tree_side\6 \brep\vstring\1\4\0\0\16:t:gs?$?/..\0\r?:gs?^??\vconcat\ntable#nvim_tree_root_folder_modifier\27nvim_tree_quit_on_open\1\4\0\0\t.git\17node_modules\v.cache\21nvim_tree_ignore\29nvim_tree_indent_markers\27nvim_tree_hijack_netrw%nvim_tree_highlight_opened_files\28nvim_tree_hide_dotfiles\24nvim_tree_gitignore\21nvim_tree_git_hl\21nvim_tree_follow\28nvim_tree_disable_netrw\24nvim_tree_auto_open\1\2\0\0\14dashboard\29nvim_tree_auto_ignore_ft\25nvim_tree_auto_close\27nvim_tree_allow_resize\27nvim_tree_add_trailing\18termguicolors\6o\6g\bvim\23nvim_tree_callback\1\0\r\tblue\f#5E81AC\afg\f#E5E9F0\fmagenta\f#BF616A\vyellow\f#EBCB8B\rdarkblue\f#81A1C1\abg\tNONE\ngreen\f#8FBCBB\bred\f#BF616A\vorange\f#D08770\tcyan\f#A3BE8C\tgray\f#616E88\vpurple\f#B48EAD\nwhite\f#ECEFF4\21nvim-tree.config\frequire\npcall\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    after = { "nvim-ts-rainbow" },
    config = { "\27LJ\1\2«\1\0\0\3\0\n\0\r4\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\3\0003\2\4\0:\2\5\0013\2\6\0:\2\a\0013\2\b\0:\2\t\1>\0\2\1G\0\1\0\14autopairs\1\0\1\venable\2\14highlight\1\0\2\21use_languagetree\2\venable\2\vindent\1\0\1\venable\2\1\0\1\21ensure_installed\blua\nsetup\28nvim-treesitter.configs\frequire\0" },
    loaded = false,
    needs_bufread = true,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/nvim-treesitter"
  },
  ["nvim-ts-rainbow"] = {
    config = { "\27LJ\1\2v\0\0\3\0\6\0\t4\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\4\0003\2\3\0:\2\5\1>\0\2\1G\0\1\0\frainbow\1\0\0\1\0\2\18extended_mode\2\venable\2\nsetup\28nvim-treesitter.configs\frequire\0" },
    load_after = {
      ["nvim-treesitter"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/nvim-ts-rainbow"
  },
  ["nvim-web-devicons"] = {
    after = { "galaxyline.nvim", "bufferline.nvim" },
    config = { "\27LJ\1\2£\n\0\0\b\0A\0j4\0\0\0004\1\1\0%\2\2\0>\0\3\3\14\0\0\0T\2\1ÄG\0\1\0003\2\3\0007\3\4\0013\4?\0003\5\b\0003\6\5\0007\a\6\2:\a\a\6:\6\t\0053\6\n\0007\a\6\2:\a\a\6:\6\v\0053\6\f\0007\a\r\2:\a\a\6:\6\14\0053\6\15\0007\a\r\2:\a\a\6:\6\16\0053\6\17\0007\a\18\2:\a\a\6:\6\19\0053\6\20\0007\a\21\2:\a\a\6:\6\22\0053\6\23\0007\a\21\2:\a\a\6:\6\24\0053\6\25\0007\a\26\2:\a\a\6:\6\27\0053\6\28\0007\a\29\2:\a\a\6:\6\30\0053\6\31\0007\a \2:\a\a\6:\6!\0053\6\"\0007\a\6\2:\a\a\6:\6#\0053\6$\0007\a%\2:\a\a\6:\6&\0053\6'\0007\a%\2:\a\a\6:\6(\0053\6)\0007\a%\2:\a\a\6:\6*\0053\6+\0007\a\21\2:\a\a\6:\6,\0053\6-\0007\a\r\2:\a\a\6:\6.\0053\6/\0007\a\6\2:\a\a\6:\0060\0053\0061\0007\a2\2:\a\a\6:\0063\0053\0064\0007\a5\2:\a\a\6:\0066\0053\0067\0007\a5\2:\a\a\6:\0068\0053\0069\0007\a\18\2:\a\a\6:\6:\0053\6;\0007\a\26\2:\a\a\6:\6<\0053\6=\0007\a\26\2:\a\a\6:\6>\5:\5@\4>\3\2\1G\0\1\0\roverride\1\0\0\bzip\1\0\2\ticon\bÔáÜ\tname\bzip\axz\1\0\2\ticon\bÔáÜ\tname\axz\bvue\1\0\2\ticon\bÔµÇ\tname\bvue\brpm\1\0\2\ticon\bÔåñ\tname\brpm\arb\fmagenta\1\0\2\ticon\bÓòÖ\tname\arb\ats\rdarkblue\1\0\2\ticon\bÔØ§\tname\ats\ttoml\1\0\2\ticon\bÔÄì\tname\ttoml\apy\1\0\2\ticon\bÓòÜ\tname\apy\bpng\1\0\2\ticon\bÔùé\tname\bpng\bout\1\0\2\ticon\bÓòí\tname\bout\bmp4\1\0\2\ticon\bÔÖÑ\tname\bmp4\bmp3\nwhite\1\0\2\ticon\bÔ¢Ö\tname\bmp3\blua\1\0\2\ticon\bÓò†\tname\blua\tlock\bred\1\0\2\ticon\bÔ†Ω\tname\tlock\akt\vorange\1\0\2\ticon\tÛ±àô\tname\akt\ajs\vyellow\1\0\2\ticon\bÔ†ù\tname\ajs\bjpg\1\0\2\ticon\bÔùé\tname\bjpg\tjpeg\vpurple\1\0\2\ticon\bÔùé\tname\tjpeg\thtml\ngreen\1\0\2\ticon\bÔÑª\tname\thtml\15Dockerfile\1\0\2\ticon\bÔåà\tname\15Dockerfile\bdeb\tcyan\1\0\2\ticon\bÔåÜ\tname\bdeb\bcss\1\0\2\ticon\bÓùâ\tname\bcss\6c\1\0\0\ncolor\tblue\1\0\2\ticon\bÓòû\tname\6c\nsetup\1\0\r\tblue\f#5E81AC\afg\f#E5E9F0\fmagenta\f#BF616A\vyellow\f#EBCB8B\rdarkblue\f#81A1C1\abg\tNONE\ngreen\f#8FBCBB\bred\f#BF616A\vorange\f#D08770\tcyan\f#A3BE8C\tgray\f#616E88\vpurple\f#B48EAD\nwhite\f#ECEFF4\22nvim-web-devicons\frequire\npcall\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/nvim-web-devicons"
  },
  ["orgmode.nvim"] = {
    config = { "\27LJ\1\2 \v\0\0\5\0\16\0\0194\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\3\0003\2\5\0003\3\4\0:\3\6\0023\3\a\0003\4\b\0:\4\t\3:\3\n\0023\3\v\0:\3\f\0023\3\r\0:\3\14\2:\2\15\1>\0\2\1G\0\1\0\rmappings\borg\1\0\24\20org_meta_return\17<Leader><CR>#org_forward_heading_same_level\a]]\15org_export\15<Leader>oe\29org_next_visible_heading\6}\27org_toggle_archive_tag\15<Leader>oA\24org_move_subtree_up\15<Leader>oK$org_backward_heading_same_level\a[[\22org_increase_date\n<C-a>\25org_set_tags_command\15<Leader>ot\26org_move_subtree_down\15<Leader>oJ\24org_archive_subtree\15<Leader>o$\28org_insert_todo_heading\16<Leader>oiT,org_insert_todo_heading_respect_content\16<Leader>oit\14org_cycle\n<TAB>\22org_decrease_date\n<C-x>\24org_promote_subtree\a<s\24org_toggle_checkbox\14<C-Space>\18org_do_demote\a>>\23org_demote_subtree\a>s'org_insert_heading_respect_content\16<Leader>oih\21org_global_cycle\f<S-TAB>\19org_do_promote\a<<!org_previous_visible_heading\6{\22org_open_at_point\15<Leader>oo\fcapture\1\0\4\26org_capture_show_help\6?\21org_capture_kill\15<Leader>ok\23org_capture_refile\15<Leader>or\25org_capture_finalize\n<C-c>\vagenda\20org_agenda_goto\1\2\0\0\n<TAB>\1\0\r\20org_agenda_redo\6r\26org_agenda_goto_today\6.\25org_agenda_week_view\avw\20org_agenda_todo\6t\25org_agenda_switch_to\t<CR>\21org_agenda_later\6f\25org_agenda_show_help\6?\25org_agenda_year_view\avy\25org_agenda_goto_date\6J\26org_agenda_month_view\avm\24org_agenda_day_view\avd\20org_agenda_quit\6q\23org_agenda_earlier\6b\vglobal\1\0\0\1\0\2\16org_capture\15<Leader>oc\15org_agenda\15<Leader>oa\1\0\5\27org_hide_leading_stars\2\27org_default_notes_file\20~/org/notes.org\21org_agenda_files\f~/org/*\30org_hide_emphasis_markers\2$org_highlight_latex_and_related\rentities\nsetup\forgmode\frequire\0" },
    loaded = false,
    needs_bufread = true,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/orgmode.nvim"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["telescope-fzf-native.nvim"] = {
    loaded = true,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/start/telescope-fzf-native.nvim"
  },
  ["telescope.nvim"] = {
    commands = { "Telescope" },
    config = { "\27LJ\1\2M\0\0\a\2\2\0\v4\0\0\0+\1\0\0>\0\2\4T\3\4Ä+\5\1\0007\5\1\5\16\6\4\0>\5\2\1A\3\3\3N\3˙G\0\1\0\2¿\1¿\19load_extension\vipairs¡\n\1\0\a\0,\0K4\0\0\0004\1\1\0%\2\2\0>\0\3\3\14\0\0\0T\2\1Ä0\0CÄ7\2\3\0013\3#\0003\4\5\0003\5\4\0:\5\6\0043\5\b\0003\6\a\0:\6\t\0053\6\n\0:\6\v\5:\5\f\0044\5\1\0%\6\r\0>\5\2\0027\5\14\5:\5\15\0042\5\0\0:\5\16\0044\5\1\0%\6\r\0>\5\2\0027\5\17\5:\5\18\0043\5\19\0:\5\20\0042\5\0\0:\5\21\0043\5\22\0:\5\23\0043\5\24\0:\5\25\0044\5\1\0%\6\26\0>\5\2\0027\5\27\0057\5\28\5:\5\29\0044\5\1\0%\6\26\0>\5\2\0027\5\30\0057\5\28\5:\5\31\0044\5\1\0%\6\26\0>\5\2\0027\5 \0057\5\28\5:\5!\0044\5\1\0%\6\26\0>\5\2\0027\5\"\5:\5\"\4:\4$\0033\4&\0003\5%\0:\5'\4:\4(\3>\2\2\0013\2)\0%\3*\0004\4\0\0001\5+\0>\4\2\0010\0\0ÄG\0\1\0G\0\1\0\0.\"extensions\", \"telescope-fzf-native.nvim\"\1\4\0\0\vthemes\nterms\bfzf\15extensions\bfzf\1\0\0\1\0\4\28override_generic_sorter\1\14case_mode\15smart_case\25override_file_sorter\2\nfuzzy\2\rdefaults\1\0\0\27buffer_previewer_maker\21qflist_previewer\22vim_buffer_qflist\19grep_previewer\23vim_buffer_vimgrep\19file_previewer\bnew\19vim_buffer_cat\25telescope.previewers\fset_env\1\0\1\14COLORTERM\14truecolor\16borderchars\1\t\0\0\b‚îÄ\b‚îÇ\b‚îÄ\b‚îÇ\b‚ï≠\b‚ïÆ\b‚ïØ\b‚ï∞\vborder\17path_display\1\2\0\0\rabsolute\19generic_sorter\29get_generic_fuzzy_sorter\25file_ignore_patterns\16file_sorter\19get_fuzzy_file\22telescope.sorters\18layout_config\rvertical\1\0\1\vmirror\1\15horizontal\1\0\3\vheight\4ö≥ÊÃ\tô≥¶ˇ\3\19preview_cutoff\3x\nwidth\4◊«¬Î\3äÆØˇ\3\1\0\3\18preview_width\4ö≥ÊÃ\tô≥Üˇ\3\20prompt_position\btop\18results_width\4ö≥ÊÃ\tô≥¶ˇ\3\22vimgrep_arguments\1\0\n\23selection_strategy\nreset\20layout_strategy\15horizontal\ruse_less\2\17entry_prefix\a  \21sorting_strategy\14ascending\18prompt_prefix\t Œª \17initial_mode\vinsert\rwinblend\3\0\19color_devicons\2\20selection_caret\b > \1\b\0\0\arg\18--color=never\17--no-heading\20--with-filename\18--line-number\r--column\17--smart-case\nsetup\14telescope\frequire\npcall\0" },
    loaded = false,
    needs_bufread = true,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/telescope.nvim"
  },
  ["twilight.nvim"] = {
    commands = { "Twilight", "TwilightEnable" },
    config = { "\27LJ\1\2}\0\0\3\0\6\0\t4\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\4\0003\2\3\0:\2\5\1>\0\2\1G\0\1\0\fdimming\1\0\2\fcontext\3\0\15treesitter\2\1\0\2\nalpha\4\0ÄÄ¿˛\3\rinactive\2\nsetup\rtwilight\frequire\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/twilight.nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: gitsigns.nvim
time([[Config for gitsigns.nvim]], true)
try_loadstring("\27LJ\1\2ˆ\f\0\0\4\0\26\0\0294\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\14\0003\2\4\0003\3\3\0:\3\5\0023\3\6\0:\3\a\0023\3\b\0:\3\t\0023\3\n\0:\3\v\0023\3\f\0:\3\r\2:\2\15\0013\2\16\0003\3\17\0:\3\18\0023\3\19\0:\3\20\2:\2\21\0013\2\22\0:\2\23\0013\2\24\0:\2\25\1>\0\2\1G\0\1\0\tyadm\1\0\1\venable\2\16watch_index\1\0\2\rinterval\3Ë\a\17follow_files\2\fkeymaps\tn [c\1\2\1\0H&diff ? '[c' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'\texpr\2\tn ]c\1\2\1\0H&diff ? ']c' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'\texpr\2\1\0\r\fnoremap\2\17v <leader>hrT<cmd>lua require\"gitsigns\".reset_hunk({vim.fn.line(\".\"), vim.fn.line(\"v\")})<CR>\17n <leader>hR2<cmd>lua require\"gitsigns\".reset_buffer()<CR>\17n <leader>hs0<cmd>lua require\"gitsigns\".stage_hunk()<CR>\17n <leader>hb4<cmd>lua require\"gitsigns\".blame_line(true)<CR>\to ih::<C-U>lua require\"gitsigns.actions\".select_hunk()<CR>\tx ih::<C-U>lua require\"gitsigns.actions\".select_hunk()<CR>\17n <leader>hr0<cmd>lua require\"gitsigns\".reset_hunk()<CR>\17n <leader>hU8<cmd>lua require\"gitsigns\".reset_buffer_index()<CR>\17v <leader>hsT<cmd>lua require\"gitsigns\".stage_hunk({vim.fn.line(\".\"), vim.fn.line(\"v\")})<CR>\17n <leader>hp2<cmd>lua require\"gitsigns\".preview_hunk()<CR>\17n <leader>hS2<cmd>lua require\"gitsigns\".stage_buffer()<CR>\17n <leader>hu5<cmd>lua require\"gitsigns\".undo_stage_hunk()<CR>\nsigns\1\0\5\20update_debounce\3»\1\vlinehl\1\nnumhl\1\15signcolumn\2\14word_diff\1\17changedelete\1\0\4\ttext\6~\nnumhl\21GitSignsChangeNr\vlinehl\21GitSignsChangeLn\ahl\19GitSignsChange\14topdelete\1\0\4\ttext\b‚Äæ\nnumhl\21GitSignsDeleteNr\vlinehl\21GitSignsDeleteLn\ahl\19GitSignsDelete\vdelete\1\0\4\ttext\6_\nnumhl\21GitSignsDeleteNr\vlinehl\21GitSignsDeleteLn\ahl\19GitSignsDelete\vchange\1\0\4\ttext\b‚îÇ\nnumhl\21GitSignsChangeNr\vlinehl\21GitSignsChangeLn\ahl\19GitSignsChange\badd\1\0\0\1\0\4\ttext\b‚îÇ\nnumhl\18GitSignsAddNr\vlinehl\18GitSignsAddLn\ahl\16GitSignsAdd\nsetup\rgitsigns\frequire\0", "config", "gitsigns.nvim")
time([[Config for gitsigns.nvim]], false)
-- Config for: alpha-nvim
time([[Config for alpha-nvim]], true)
try_loadstring("\27LJ\1\2˛\19\0\0\b\0\14\1\0244\0\0\0%\1\1\0>\0\2\0024\1\0\0%\2\2\0>\1\2\0027\2\3\0017\2\4\0023\3\6\0:\3\5\0027\2\3\0017\2\a\0022\3\3\0007\4\b\1%\5\t\0%\6\n\0%\a\v\0>\4\4\0<\4\0\0:\3\5\0027\2\f\0007\3\r\1>\2\2\1G\0\1\0\topts\nsetup\f:qa<CR>\bÔåå\tv0.9\vbutton\fbuttons\1#\0\0I            :h-                                  Nhy`               I           -mh.                           h.    `Ndho               I           hmh+                          oNm.   oNdhh               I          `Nmhd`                        /NNmd  /NNhhd               I          -NNhhy                      `hMNmmm`+NNdhhh               I          .NNmhhs              ```....`..-:/./mNdhhh+               I           mNNdhhh-     `.-::///+++////++//:--.`-/sd`               I           oNNNdhhdo..://++//++++++/+++//++///++/-.`                I      y.   `mNNNmhhhdy+/++++//+/////++//+++///++////-` `/oos:       I .    Nmy:  :NNNNmhhhhdy+/++/+++///:.....--:////+++///:.`:s+        I h-   dNmNmy oNNNNNdhhhhy:/+/+++/-         ---:/+++//++//.`         I hd+` -NNNy`./dNNNNNhhhh+-://///    -+oo:`  ::-:+////++///:`        I /Nmhs+oss-:++/dNNNmhho:--::///    /mmmmmo  ../-///++///////.       I  oNNdhhhhhhhs//osso/:---:::///    /yyyyso  ..o+-//////////:/.      I   /mNNNmdhhhh/://+///::://////     -:::- ..+sy+:////////::/:/.     I     /hNNNdhhs--:/+++////++/////.      ..-/yhhs-/////////::/::/`    I       .ooo+/-::::/+///////++++//-/ossyyhhhhs/:///////:::/::::/:    I       -///:::::::////++///+++/////:/+ooo+/::///////.::://::---+`   I       /////+//++++/////+////-..//////////::-:::--`.:///:---:::/:   I       //+++//++++++////+++///::--                 .::::-------::   I       :/++++///////////++++//////.                -:/:----::../-   I       -/++++//++///+//////////////               .::::---:::-.+`   I       `////////////////////////////:.            --::-----...-/    I        -///://////////////////////::::-..      :-:-:-..-::.`.+`    I         :/://///:///::://::://::::::/:::::::-:---::-.-....``/- -   I           ::::://::://::::::::::::::----------..-:....`.../- -+oo/ I            -/:::-:::::---://:-::-::::----::---.-.......`-/.      ``I           s-`::--:::------:////----:---.-:::...-.....`./:          I          yMNy.`::-.--::..-dmmhhhs-..-.-.......`.....-/:`           I         oMNNNh. `-::--...:NNNdhhh/.--.`..``.......:/-              I        :dy+:`      .-::-..NNNhhd+``..`...````.-::-`                I                        .-:mNdhh:.......--::::-`                    I                           yNh/..------..`                          I                                                                    \bval\vheader\fsection\27alpha.themes.dashboard\nalpha\frequire\3ÄÄ¿ô\4\0", "config", "alpha-nvim")
time([[Config for alpha-nvim]], false)
-- Load plugins in order defined by `after`
time([[Sequenced loading]], true)
vim.cmd [[ packadd nvim-lspconfig ]]

-- Config for: nvim-lspconfig
try_loadstring("\27LJ\1\2A\2\0\3\1\3\0\a4\0\0\0007\0\1\0007\0\2\0+\1\0\0C\2\0\0=\0\1\1G\0\1\0\1¿\24nvim_buf_set_keymap\bapi\bvimA\2\0\3\1\3\0\a4\0\0\0007\0\1\0007\0\2\0+\1\0\0C\2\0\0=\0\1\1G\0\1\0\1¿\24nvim_buf_set_option\bapi\bvimò\v\1\2\n\0*\0u1\2\0\0001\3\1\0\16\4\3\0%\5\2\0%\6\3\0>\4\3\0013\4\4\0\16\5\2\0%\6\5\0%\a\6\0%\b\a\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\b\0%\b\t\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\n\0%\b\v\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\f\0%\b\r\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\14\0%\b\15\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\16\0%\b\17\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\18\0%\b\19\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\20\0%\b\21\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\22\0%\b\23\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\24\0%\b\25\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\26\0%\b\27\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\28\0%\b\29\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\30\0%\b\31\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a \0%\b!\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a\"\0%\b#\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a$\0%\b%\0\16\t\4\0>\5\5\1\16\5\2\0%\6\5\0%\a&\0%\b'\0\16\t\4\0>\5\5\1\16\5\2\0%\6(\0%\a\26\0%\b)\0\16\t\4\0>\5\5\0010\0\0ÄG\0\1\0001<cmd>lua vim.lsp.buf.range_code_action()<CR>\6v*<cmd>lua vim.lsp.buf.formatting()<CR>\r<space>f2<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>\r<space>q0<cmd>lua vim.lsp.diagnostic.goto_next()<CR>\a]d0<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>\a[d<<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>\r<space>e*<cmd>lua vim.lsp.buf.references()<CR>\agr+<cmd>lua vim.lsp.buf.code_action()<CR>\14<space>ca&<cmd>lua vim.lsp.buf.rename()<CR>\14<space>rn/<cmd>lua vim.lsp.buf.type_definition()<CR>\r<space>DJ<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>\14<space>wl7<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>\14<space>wr4<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>\14<space>wa.<cmd>lua vim.lsp.buf.signature_help()<CR>\agk.<cmd>lua vim.lsp.buf.implementation()<CR>\agi%<cmd>lua vim.lsp.buf.hover()<CR>\6K*<cmd>lua vim.lsp.buf.definition()<CR>\agd+<cmd>lua vim.lsp.buf.declaration()<CR>\agD\6n\1\0\2\vsilent\2\fnoremap\2\27v:lua.vim.lsp.omnifunc\romnifunc\0\0–\4\0\0\14\4\29\0D+\0\0\0007\0\0\0>\0\1\1+\0\0\0007\0\1\0>\0\1\0024\1\2\0\16\2\0\0>\1\2\4D\0047Ä\6\5\3\0T\6\fÄ+\6\1\0006\6\5\0067\6\0\0063\a\4\0+\b\2\0:\b\5\a+\b\3\0:\b\6\a3\b\a\0:\b\b\a>\6\2\1T\6)Ä\a\5\3\0T\6'Ä+\6\1\0006\6\5\0067\6\0\0063\a\t\0+\b\2\0:\b\5\a+\b\3\0:\b\6\a3\b\n\0:\b\b\a3\b\26\0003\t\14\0003\n\f\0003\v\v\0:\v\r\n:\n\15\t3\n\21\0002\v\0\b4\f\16\0007\f\17\f7\f\18\f%\r\19\0>\f\2\2)\r\2\0009\r\f\v4\f\16\0007\f\17\f7\f\18\f%\r\20\0>\f\2\2)\r\2\0009\r\f\v:\v\22\n:\n\23\t3\n\24\0:\n\25\t:\t\27\b:\b\28\a>\6\2\1B\4\3\3N\4«G\0\1\0\3¿\1¿\4¿\5¿\rsettings\bLua\1\0\0\14telemetry\1\0\1\venable\1\14workspace\flibrary\1\0\2\15maxPreload\3†ç\6\20preloadFileSize\3êN\28$VIMRUNTIME/lua/vim/lsp\20$VIMRUNTIME/lua\vexpand\afn\bvim\16diagnostics\1\0\0\fglobals\1\0\0\1\2\0\0\bvim\1\0\1\26debounce_text_changes\3Ù\3\1\0\0\nflags\1\0\1\26debounce_text_changes\3Ù\3\17capabilities\14on_attach\1\0\0\blua\npairs\22installed_servers\nsetup5\0\0\2\1\3\0\a+\0\0\0>\0\1\0014\0\0\0007\0\1\0%\1\2\0>\0\2\1G\0\1\0\6¿\fbufdo e\bcmd\bvimà\1\0\2\a\0\b\0\0144\2\0\0007\2\1\0027\2\2\2%\3\3\0\16\4\0\0$\3\4\0033\4\4\0:\1\5\4%\5\6\0\16\6\0\0$\5\6\5:\5\a\4>\2\3\1G\0\1\0\nnumhl\25LspDiagnosticsDefaul\ttext\1\0\0\23LspDiagnosticsSign\16sign_define\afn\bvim√\1\0\3\a\0\t\0\30\16\4\0\0007\3\0\0%\5\1\0>\3\3\2\15\0\3\0T\4\1ÄG\0\1\0004\3\2\0007\3\3\0037\3\4\0037\3\5\3\5\1\3\0T\3\6Ä4\3\2\0007\3\6\0037\3\a\3\16\4\0\0>\3\2\1T\3\nÄ4\3\2\0007\3\6\0037\3\b\0032\4\3\0002\5\3\0;\0\1\5;\5\1\4)\5\2\0002\6\0\0>\3\4\1G\0\1\0\14nvim_echo\21nvim_err_writeln\bapi\nERROR\vlevels\blog\bvim\14exit code\nmatchÛ\t\1\0\r\0008\0â\0014\0\0\0004\1\1\0%\2\2\0>\0\3\0034\2\0\0004\3\1\0%\4\3\0>\2\3\3\14\0\0\0T\4\3Ä\14\0\2\0T\4\1Ä0\0{Ä1\4\4\0004\5\5\0007\5\6\0057\5\a\0057\5\b\5>\5\1\0027\6\t\0057\6\n\0067\6\v\0063\a\r\0:\a\f\0067\6\t\0057\6\n\0067\6\v\6)\a\2\0:\a\14\0067\6\t\0057\6\n\0067\6\v\6)\a\2\0:\a\15\0067\6\t\0057\6\n\0067\6\v\6)\a\2\0:\a\16\0067\6\t\0057\6\n\0067\6\v\6)\a\2\0:\a\17\0067\6\t\0057\6\n\0067\6\v\6)\a\2\0:\a\18\0067\6\t\0057\6\n\0067\6\v\6)\a\2\0:\a\19\0067\6\t\0057\6\n\0067\6\v\0063\a\22\0003\b\21\0:\b\23\a:\a\20\0067\6\t\0057\6\n\0067\6\v\0063\a\26\0003\b\25\0:\b\27\a:\a\24\0061\6\28\0\16\a\6\0>\a\1\0011\a\30\0:\a\29\0031\a\31\0\16\b\a\0%\t \0%\n!\0>\b\3\1\16\b\a\0%\t\"\0%\n#\0>\b\3\1\16\b\a\0%\t$\0%\n%\0>\b\3\1\16\b\a\0%\t&\0%\n'\0>\b\3\0014\b\5\0007\b\6\b7\b(\b4\t\5\0007\t\6\t7\t*\t4\n\5\0007\n\6\n7\n+\n7\n,\n3\v.\0003\f-\0:\f/\v>\t\3\2:\t)\b4\b\5\0007\b\6\b7\b(\b4\t\5\0007\t\6\t7\t*\t4\n\5\0007\n\6\n7\n(\n7\n1\n3\v2\0>\t\3\2:\t0\b4\b\5\0007\b\6\b7\b(\b4\t\5\0007\t\6\t7\t*\t4\n\5\0007\n\6\n7\n(\n7\n4\n3\v5\0>\t\3\2:\t3\b4\b\5\0001\t7\0:\t6\b0\0\0ÄG\0\1\0G\0\1\0\0\vnotify\1\0\1\vborder\vsingle\19signature_help\31textDocument/signatureHelp\1\0\1\vborder\vsingle\nhover\23textDocument/hover\17virtual_text\1\0\3\14underline\2\nsigns\2\21update_in_insert\1\1\0\2\fspacing\3\0\vprefix\bÔëÖ\27on_publish_diagnostics\15diagnostic\twith$textDocument/publishDiagnostics\rhandlers\bÔÅ±\fWarning\bÔ†¥\tHint\bÔüª\16Information\bÔôò\nError\0\0\22post_install_hook\0\15properties\1\0\0\1\4\0\0\18documentation\vdetail\24additionalTextEdits\19resolveSupport\rvalueSet\1\0\0\1\2\0\0\3\1\15tagSupport\28commitCharactersSupport\22deprecatedSupport\24labelDetailsSupport\25insertReplaceSupport\21preselectSupport\19snippetSupport\1\3\0\0\rmarkdown\14plaintext\24documentationFormat\19completionItem\15completion\17textDocument\29make_client_capabilities\rprotocol\blsp\bvim\0\15lspinstall\14lspconfig\frequire\npcall\0", "config", "nvim-lspconfig")

vim.cmd [[ packadd lsp_signature.nvim ]]

-- Config for: lsp_signature.nvim
try_loadstring("\27LJ\1\2ó\2\0\0\5\0\a\0\f4\0\0\0004\1\1\0%\2\2\0>\0\3\3\15\0\0\0T\2\5Ä7\2\3\0013\3\4\0003\4\5\0:\4\6\3>\2\2\1G\0\1\0\17handler_opts\1\0\1\vborder\vsingle\1\0\f\15max_height\3\22\14doc_lines\3\2\20floating_window\2\tbind\2\16hint_prefix\tÔüª \ffix_pos\2\vzindex\3»\1\17hi_parameter\vSearch\16hint_scheme\vString\16hint_enable\2\14max_width\3x\fpadding\5\nsetup\18lsp_signature\frequire\npcall\0", "config", "lsp_signature.nvim")

vim.cmd [[ packadd nord.nvim ]]

-- Config for: nord.nvim
try_loadstring("\27LJ\1\2≈\1\0\0\2\0\b\0\0214\0\0\0007\0\1\0)\1\1\0:\1\2\0004\0\0\0007\0\1\0)\1\1\0:\1\3\0004\0\0\0007\0\1\0)\1\2\0:\1\4\0004\0\0\0007\0\1\0)\1\1\0:\1\5\0004\0\0\0007\0\6\0%\1\a\0>\0\2\1G\0\1\0\21colorscheme nord\bcmd\28nord_disable_background nord_cursorline_transparent\18nord_contrast\17nord_borders\6g\bvim\0", "config", "nord.nvim")

vim.cmd [[ packadd nvim-web-devicons ]]

-- Config for: nvim-web-devicons
try_loadstring("\27LJ\1\2£\n\0\0\b\0A\0j4\0\0\0004\1\1\0%\2\2\0>\0\3\3\14\0\0\0T\2\1ÄG\0\1\0003\2\3\0007\3\4\0013\4?\0003\5\b\0003\6\5\0007\a\6\2:\a\a\6:\6\t\0053\6\n\0007\a\6\2:\a\a\6:\6\v\0053\6\f\0007\a\r\2:\a\a\6:\6\14\0053\6\15\0007\a\r\2:\a\a\6:\6\16\0053\6\17\0007\a\18\2:\a\a\6:\6\19\0053\6\20\0007\a\21\2:\a\a\6:\6\22\0053\6\23\0007\a\21\2:\a\a\6:\6\24\0053\6\25\0007\a\26\2:\a\a\6:\6\27\0053\6\28\0007\a\29\2:\a\a\6:\6\30\0053\6\31\0007\a \2:\a\a\6:\6!\0053\6\"\0007\a\6\2:\a\a\6:\6#\0053\6$\0007\a%\2:\a\a\6:\6&\0053\6'\0007\a%\2:\a\a\6:\6(\0053\6)\0007\a%\2:\a\a\6:\6*\0053\6+\0007\a\21\2:\a\a\6:\6,\0053\6-\0007\a\r\2:\a\a\6:\6.\0053\6/\0007\a\6\2:\a\a\6:\0060\0053\0061\0007\a2\2:\a\a\6:\0063\0053\0064\0007\a5\2:\a\a\6:\0066\0053\0067\0007\a5\2:\a\a\6:\0068\0053\0069\0007\a\18\2:\a\a\6:\6:\0053\6;\0007\a\26\2:\a\a\6:\6<\0053\6=\0007\a\26\2:\a\a\6:\6>\5:\5@\4>\3\2\1G\0\1\0\roverride\1\0\0\bzip\1\0\2\ticon\bÔáÜ\tname\bzip\axz\1\0\2\ticon\bÔáÜ\tname\axz\bvue\1\0\2\ticon\bÔµÇ\tname\bvue\brpm\1\0\2\ticon\bÔåñ\tname\brpm\arb\fmagenta\1\0\2\ticon\bÓòÖ\tname\arb\ats\rdarkblue\1\0\2\ticon\bÔØ§\tname\ats\ttoml\1\0\2\ticon\bÔÄì\tname\ttoml\apy\1\0\2\ticon\bÓòÜ\tname\apy\bpng\1\0\2\ticon\bÔùé\tname\bpng\bout\1\0\2\ticon\bÓòí\tname\bout\bmp4\1\0\2\ticon\bÔÖÑ\tname\bmp4\bmp3\nwhite\1\0\2\ticon\bÔ¢Ö\tname\bmp3\blua\1\0\2\ticon\bÓò†\tname\blua\tlock\bred\1\0\2\ticon\bÔ†Ω\tname\tlock\akt\vorange\1\0\2\ticon\tÛ±àô\tname\akt\ajs\vyellow\1\0\2\ticon\bÔ†ù\tname\ajs\bjpg\1\0\2\ticon\bÔùé\tname\bjpg\tjpeg\vpurple\1\0\2\ticon\bÔùé\tname\tjpeg\thtml\ngreen\1\0\2\ticon\bÔÑª\tname\thtml\15Dockerfile\1\0\2\ticon\bÔåà\tname\15Dockerfile\bdeb\tcyan\1\0\2\ticon\bÔåÜ\tname\bdeb\bcss\1\0\2\ticon\bÓùâ\tname\bcss\6c\1\0\0\ncolor\tblue\1\0\2\ticon\bÓòû\tname\6c\nsetup\1\0\r\tblue\f#5E81AC\afg\f#E5E9F0\fmagenta\f#BF616A\vyellow\f#EBCB8B\rdarkblue\f#81A1C1\abg\tNONE\ngreen\f#8FBCBB\bred\f#BF616A\vorange\f#D08770\tcyan\f#A3BE8C\tgray\f#616E88\vpurple\f#B48EAD\nwhite\f#ECEFF4\22nvim-web-devicons\frequire\npcall\0", "config", "nvim-web-devicons")

vim.cmd [[ packadd bufferline.nvim ]]

-- Config for: bufferline.nvim
try_loadstring("\27LJ\1\2D\0\0\3\1\4\0\0064\0\0\0007\0\1\0007\0\2\0+\1\0\0%\2\3\0@\0\3\0\0¿\14term_type\21nvim_buf_get_var\bapi\bvimÄ\1\1\1\4\0\4\0\0264\1\0\0001\2\1\0>\1\2\3\15\0\1\0T\3\16Ä\a\2\2\0T\3\4Ä)\3\1\0000\0\0ÄH\3\2\0T\3\rÄ\a\2\3\0T\3\4Ä)\3\1\0000\0\0ÄH\3\2\0T\3\aÄ)\3\2\0000\0\0ÄH\3\2\0T\3\3Ä)\3\2\0000\0\0ÄH\3\2\0000\0\0ÄG\0\1\0\thori\tvert\0\npcall¯\f\1\0\b\0@\0ç\0014\0\0\0004\1\1\0%\2\2\0>\0\3\3\14\0\0\0T\2\1Ä0\0ÖÄ4\2\3\0007\2\4\2%\3\5\0>\2\2\0013\2\6\0007\3\a\0013\4\r\0003\5\t\0002\6\3\0003\a\b\0;\a\1\6:\6\n\0051\6\v\0:\6\f\5:\5\14\0043\5\20\0003\6\16\0007\a\15\2:\a\17\0067\a\18\2:\a\19\6:\6\21\0053\6\23\0007\a\22\2:\a\17\0067\a\24\2:\a\19\6:\6\25\0053\6\27\0007\a\26\2:\a\17\0067\a\18\2:\a\19\6:\6\28\0053\6\29\0007\a\26\2:\a\17\0067\a\18\2:\a\19\6:\6\30\0053\6\31\0007\a\26\2:\a\17\0067\a\18\2:\a\19\6:\6 \0053\6!\0007\a\26\2:\a\17\0067\a\18\2:\a\19\6:\6\"\0053\6#\0007\a\26\2:\a\17\0067\a\18\2:\a\19\6:\6$\0053\6&\0007\a%\2:\a\17\0067\a\24\2:\a\19\6:\6'\0053\6(\0007\a\15\2:\a\17\0067\a\18\2:\a\19\6:\6)\0053\6*\0007\a\24\2:\a\17\0067\a\24\2:\a\19\6:\6+\0053\6,\0007\a%\2:\a\17\0067\a\18\2:\a\19\6:\6-\0053\6.\0007\a%\2:\a\17\0067\a\18\2:\a\19\6:\6/\0053\0061\0007\a0\2:\a\17\0067\a\24\2:\a\19\6:\0062\0053\0063\0007\a\18\2:\a\17\0067\a\18\2:\a\19\6:\0064\0053\0065\0007\a\18\2:\a\17\0067\a\18\2:\a\19\6:\0066\0053\0067\0007\a\18\2:\a\17\0067\a\18\2:\a\19\6:\0068\0053\0069\0007\a\15\2:\a\17\0067\a\26\2:\a\19\6:\6:\0053\6;\0007\a\18\2:\a\17\0067\a\15\2:\a\19\6:\6<\0053\6=\0007\a%\2:\a\17\0067\a\24\2:\a\19\6:\6>\5:\5?\4>\3\2\1G\0\1\0G\0\1\0\15highlights\14tab_close\1\0\0\17tab_selected\1\0\0\btab\1\0\0\23separator_selected\1\0\0\22separator_visible\1\0\0\14separator\1\0\0\22modified_selected\1\0\0\ngreen\21modified_visible\1\0\0\rmodified\1\0\0\23indicator_selected\1\0\0\tfill\1\0\0\26close_button_selected\1\0\0\bred\25close_button_visible\1\0\0\17close_button\1\0\0\21error_diagnostic\1\0\0\nerror\1\0\0\19buffer_visible\1\0\0\tgray\20buffer_selected\nblack\1\0\1\bgui\tbold\nwhite\15background\1\0\0\nguibg\vblack2\nguifg\1\0\0\afg\foptions\1\0\0\18custom_filter\0\foffsets\1\0\16\25enforce_regular_tabs\1\22left_trunc_marker\bÔÇ®\rtab_size\3\20\16diagnostics\1\24show_tab_indicators\2\28show_buffer_close_icons\2\20separator_style\tthin\22buffer_close_icon\bÔôï\20max_name_length\3\14\15close_icon\bÔåå\tview\16multiwindow\23right_trunc_marker\bÔÇ©\22max_prefix_length\3\r\27always_show_bufferline\1\20show_close_icon\2\18modified_icon\bÔëÑ\1\0\3\ttext\5\fpadding\3\1\rfiletype\rNvimTree\nsetup\1\0\15\tblue\f#5E81AC\nblack\f#3b4252\abg\tNONE\vyellow\f#EBCB8B\rdarkblue\f#81A1C1\vblack2\f#2E3440\ngreen\f#8FBCBB\fmagenta\f#BF616A\vorange\f#D08770\tcyan\f#A3BE8C\afg\f#E5E9F0\tgray\f#616E88\bred\f#BF616A\vpurple\f#B48EAD\nwhite\f#ECEFF4Bfunction! Doom_bufferline_quitvim(a,b,c,d) \n qa \n endfunction\bcmd\bvim\15bufferline\frequire\npcall\0", "config", "bufferline.nvim")

vim.cmd [[ packadd galaxyline.nvim ]]

-- Config for: galaxyline.nvim
try_loadstring("\27LJ\1\2ƒ\1\0\0\3\1\16\0 3\0\1\0+\1\0\0007\1\0\1:\1\2\0+\1\0\0007\1\3\1:\1\4\0+\1\0\0007\1\5\1:\1\6\0+\1\0\0007\1\a\1:\1\b\0+\1\0\0007\1\a\1:\1\t\0+\1\0\0007\1\a\1:\1\n\0+\1\0\0007\1\v\1:\1\f\0004\1\r\0007\1\14\0017\1\15\1>\1\1\0026\1\1\0\v\1\0\0T\2\2Ä+\2\0\0007\1\v\2H\1\2\0\3¿\tmode\afn\bvim\6R\bred\6v\6\22\6V\fmagenta\6c\vorange\6i\ngreen\6n\1\0\0\tcyan˘\1\0\0\5\1\t\0\0263\0\0\0004\1\1\0007\1\2\0017\1\3\1%\2\4\0+\3\0\0>\3\1\2$\2\3\2>\1\2\0014\1\1\0007\1\5\0017\1\6\1>\1\1\0026\1\1\0\v\1\0\0T\2\5Ä4\2\1\0007\2\5\0027\2\6\2>\2\1\2\16\1\2\0%\2\a\0\16\3\1\0%\4\b\0$\2\4\2H\2\2\0\4¿\6 \a  \tmode\afn\27hi GalaxyViMode guibg=\17nvim_command\bapi\bvim\1\0\a\6\22\rDOOM Ô¶à\6c\rDOOM Ôíâ\6V\rDOOM Ô¶à\6R\rDOOM Ôê¢\6n\fDOOM Œª\6v\rDOOM Ô¶à\6i\rDOOM ÓüÖ\21\0\0\1\0\1\0\2%\0\0\0H\0\2\0\n Ôû° ∞\1\0\0\6\0\b\0\0254\0\0\0%\1\1\0>\0\2\0027\1\2\0>\1\1\0024\2\3\0007\2\4\2\16\3\1\0>\2\2\2'\3\28\0\1\3\2\0T\2\tÄ4\2\3\0007\2\5\2\16\3\1\0'\4\1\0'\5\25\0>\2\4\2%\3\6\0$\2\3\2H\2\2\0\16\2\1\0%\3\a\0$\2\3\2H\2\2\0\6 \b...\bsub\blen\vstring\19get_git_branch\29galaxyline.providers.vcs\frequire\20\0\0\1\0\1\0\2%\0\0\0H\0\2\0\tÓÇ∫ \17\0\0\1\0\1\0\2%\0\0\0H\0\2\0\6 \17\0\0\1\0\1\0\2%\0\0\0H\0\2\0\6 '\0\0\1\0\3\0\0044\0\0\0007\0\1\0007\0\2\0H\0\2\0\rfiletype\abo\bvim\21\0\0\1\0\1\0\2%\0\0\0H\0\2\0\n Ôåå ﬂ\19\1\0\v\0`\0∂\0024\0\0\0%\1\1\0>\0\2\0024\1\0\0%\2\2\0>\1\2\0027\2\3\0003\3\5\0:\3\4\0003\3\6\0001\4\a\0007\5\b\0023\6\16\0003\a\n\0001\b\t\0:\b\v\a2\b\3\0007\t\f\3;\t\1\b7\t\f\3;\t\2\b:\b\r\a2\b\3\0007\t\14\3;\t\1\b7\t\14\3;\t\2\b:\b\15\a:\a\17\6;\6\1\0057\5\b\0023\6\21\0003\a\18\0002\b\3\0004\t\0\0%\n\19\0>\t\2\0027\t\20\t;\t\1\b7\t\14\3;\t\2\b:\b\r\a:\a\22\6;\6\2\0057\5\b\0023\6\26\0003\a\23\0002\b\3\0007\t\24\3;\t\1\b7\t\14\3;\t\2\b:\b\r\a2\b\3\0007\t\14\3;\t\1\b7\t\25\3;\t\2\b:\b\15\a:\a\27\6;\6\3\0057\5\b\0023\6!\0003\a\29\0001\b\28\0:\b\v\a7\b\30\1:\b\31\a2\b\3\0007\t \3;\t\1\b7\t\25\3;\t\2\b:\b\r\a:\a\"\6;\6\4\0057\5\b\0023\6%\0003\a$\0001\b#\0:\b\v\a7\b\30\1:\b\31\a2\b\3\0007\t\24\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a:\a&\6;\6\5\0057\5\b\0023\6)\0003\a'\0007\b\30\1:\b\31\a2\b\3\0007\t(\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a:\a*\6;\6\6\0057\5\b\0023\6-\0003\a+\0007\b\30\1:\b\31\a2\b\3\0007\t,\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a:\a.\6;\6\a\0057\5\b\0023\0060\0003\a/\0007\b\30\1:\b\31\a2\b\3\0007\t \3;\t\1\b7\t\25\3;\t\2\b:\b\r\a:\a1\6;\6\b\0057\5\b\0023\0065\0003\a3\0001\b2\0:\b\v\a2\b\3\0007\t4\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a:\a6\6;\6\t\0057\5\b\0023\0068\0003\a7\0002\b\3\0007\t \3;\t\1\b7\t4\3;\t\2\b:\b\r\a:\a9\6;\6\n\0057\5\b\0023\6<\0003\a;\0001\b:\0:\b\v\a2\b\3\0007\t\25\3;\t\1\b7\t4\3;\t\2\b:\b\r\a:\a=\6;\6\v\0057\5\b\0023\6?\0003\a>\0002\b\3\0007\t,\3;\t\1\b7\t4\3;\t\2\b:\b\r\a:\a@\6;\6\f\0057\5\b\0023\6B\0003\aA\0002\b\3\0007\t\24\3;\t\1\b7\t4\3;\t\2\b:\b\r\a:\aC\6;\6\r\0057\5\b\0023\6F\0003\aE\0001\bD\0:\b\v\a2\b\3\0007\t4\3;\t\1\b7\t4\3;\t\2\b:\b\r\a:\a=\6;\6\14\0057\5\b\0023\6I\0003\aG\0002\b\3\0007\tH\3;\t\1\b7\t4\3;\t\2\b:\b\r\a2\b\3\0007\t4\3;\t\1\b7\t\f\3;\t\2\b:\b\15\a:\aJ\6;\6\15\0057\5K\0023\6N\0003\aM\0001\bL\0:\b\v\a2\b\3\0007\t\24\3;\t\1\b7\t4\3;\t\2\b:\b\r\a2\b\3\0007\t4\3;\t\1\b7\t\f\3;\t\2\b:\b\15\a:\aO\6;\6\1\0057\5K\0023\6Q\0003\aP\0002\b\3\0007\t\24\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a2\b\3\0007\t\25\3;\t\1\b7\t4\3;\t\2\b:\b\15\a:\aR\6;\6\2\0057\5K\0023\6U\0003\aT\0001\bS\0:\b\v\a2\b\3\0007\t \3;\t\1\b7\t\14\3;\t\2\b:\b\r\a2\b\3\0007\t\14\3;\t\1\b7\t\25\3;\t\2\b:\b\15\a:\aV\6;\6\3\0057\5W\0023\6Y\0003\aX\0002\b\3\0007\t\24\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a2\b\3\0007\t\25\3;\t\1\b7\t\f\3;\t\2\b:\b\15\a:\aZ\6;\6\1\0057\5[\0023\6^\0003\a\\\0002\b\3\0007\t]\3;\t\1\b7\t\25\3;\t\2\b:\b\r\a2\b\3\0007\t\25\3;\t\1\b7\t\f\3;\t\2\b:\b\15\a:\a_\6;\6\1\0050\0\0ÄG\0\1\0\15BufferIcon\1\0\0\vyellow\1\0\2\14separator\tÓÇ∫ \rprovider\15BufferIcon\21short_line_right\15BufferType\1\0\0\1\0\2\14separator\tÓÇ∏ \rprovider\17FileTypeName\20short_line_left\tLogo\1\0\0\1\0\1\14separator\n ÓÇ∫ \0\rLineInfo\1\0\0\1\0\2\14separator\n ÓÇ∫ \rprovider\15LineColumn\15FileFormat\1\0\0\1\0\1\14separator\tÓÇ∫ \0\nright\19DiagnosticInfo\1\0\0\tblue\1\0\3\14separator\tÓÇ∏ \rprovider\19DiagnosticInfo\ticon\n ÔÅö \1\0\0\1\0\0\0\19DiagnosticHint\1\0\0\1\0\2\ticon\n ÓÄä \rprovider\19DiagnosticHint\19DiagnosticWarn\1\0\0\1\0\2\ticon\n ÔÅ± \rprovider\19DiagnosticWarn\nSpace\1\0\0\1\0\0\0\20DiagnosticError\1\0\0\1\0\2\ticon\n ÔÅó \rprovider\20DiagnosticError\fLeftEnd\1\0\0\vfilebg\1\0\0\0\15DiffRemove\1\0\0\1\0\2\ticon\tÔëò \rprovider\15DiffRemove\17DiffModified\1\0\0\vorange\1\0\2\ticon\tÔëô \rprovider\17DiffModified\fDiffAdd\1\0\0\ngreen\1\0\2\ticon\tÔëó \rprovider\fDiffAdd\14GitBranch\1\0\0\1\0\0\0\fGitIcon\1\0\0\bred\14condition\24check_git_workspace\1\0\0\0\rFileName\1\0\0\15section_bg\afg\1\0\2\14separator\tÓÇ∏ \rprovider\rFileName\rFileIcon\1\0\0\24get_file_icon_color\"galaxyline.providers.fileinfo\1\0\1\rprovider\rFileIcon\vViMode\1\0\0\24separator_highlight\16section_bg2\14highlight\abg\rprovider\1\0\1\14separator\6 \0\tleft\0\1\0\r\tblue\f#5E81AC\ngreen\f#A3BE8C\fmagenta\f#BF616A\vyellow\f#EBCB8B\rdarkblue\f#81A1C1\abg\f#3B4252\tcyan\f#8FBCBB\vorange\f#D08770\afg\f#E5E9F0\16section_bg2\f#4C566A\bred\f#BF616A\vfilebg\f#3B4252\15section_bg\f#434C5E\1\3\0\0\rNvimTree\vpacker\20short_line_list\fsection\25galaxyline.condition\15galaxyline\frequire\0", "config", "galaxyline.nvim")

time([[Sequenced loading]], false)

-- Command lazy-loads
time([[Defining lazy-load commands]], true)
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TZMinimalist lua require("packer.load")({'TrueZen.nvim'}, { cmd = "TZMinimalist", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TZAtaraxis lua require("packer.load")({'TrueZen.nvim'}, { cmd = "TZAtaraxis", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Telescope lua require("packer.load")({'telescope.nvim'}, { cmd = "Telescope", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopChar1 lua require("packer.load")({'hop'}, { cmd = "HopChar1", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopWord lua require("packer.load")({'hop'}, { cmd = "HopWord", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopChar2 lua require("packer.load")({'hop'}, { cmd = "HopChar2", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TZFocus lua require("packer.load")({'TrueZen.nvim'}, { cmd = "TZFocus", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopPattern lua require("packer.load")({'hop'}, { cmd = "HopPattern", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file NvimTreeToggle lua require("packer.load")({'nvim-tree.lua'}, { cmd = "NvimTreeToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Twilight lua require("packer.load")({'twilight.nvim'}, { cmd = "Twilight", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file HopLine lua require("packer.load")({'hop'}, { cmd = "HopLine", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TwilightEnable lua require("packer.load")({'twilight.nvim'}, { cmd = "TwilightEnable", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file NvimTreeFocus lua require("packer.load")({'nvim-tree.lua'}, { cmd = "NvimTreeFocus", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
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
vim.cmd [[au BufRead * ++once lua require("packer.load")({'nvim-treesitter', 'indent-blankline.nvim', 'nvim-colorizer.lua'}, { event = "BufRead *" }, _G.packer_plugins)]]
time([[Defining lazy-load event autocommands]], false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time([[Sourcing ftdetect script at: /home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org.vim]], true)
vim.cmd [[source /home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org.vim]]
time([[Sourcing ftdetect script at: /home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org.vim]], false)
time([[Sourcing ftdetect script at: /home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org_archive.vim]], true)
vim.cmd [[source /home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org_archive.vim]]
time([[Sourcing ftdetect script at: /home/shaunsingh0207/.local/share/nvim/site/pack/packer/opt/orgmode.nvim/ftdetect/org_archive.vim]], false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
