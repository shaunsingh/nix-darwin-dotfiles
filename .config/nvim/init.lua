local opt = vim.opt
local g = vim.g

--options
opt.fillchars = { eob = " " }
opt.termguicolors = true
opt.whichwrap:append "<>hl"
opt.undofile = true
opt.swapfile = false
opt.ruler = true
opt.ignorecase = true
opt.splitbelow = true
opt.splitright = true
opt.cursorline = true
opt.mouse = "a"
opt.signcolumn = "yes"
opt.updatetime = 500
opt.timeoutlen = 500
opt.clipboard = "unnamedplus"
opt.scrolloff = 3
opt.lazyredraw = true
opt.linebreak = true
opt.number = false
opt.numberwidth = 2
opt.expandtab = true
opt.shiftwidth = 4
opt.smartindent = true
opt.shortmess:append("casI") --disable intro

--mappings
local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

g.mapleader = " "                                                     --leader
map('i', 'jk', '<esc>')                                               --jk to exit
map('c', 'jk', '<C-C>')
map('n', ';', ':')                                                     --semicolon to enter command mode
map('n', 'j', 'gj')                                                    --move by visual line not actual line
map('n', 'k', 'gk')
map('n', '<leader>ww', '<cmd>HopWord<CR>')                              --easymotion/hop
map('n', '<leader>l', '<cmd>HopLine<CR>')
map('n', '<leader>/', '<cmd>HopPattern<CR>')
map('n', '<leader>fr', '<cmd>Telescope oldfiles<CR>')                   --fuzzy
map('n', '<leader>.', '<cmd>Telescope find_files<CR>')
map('n', '<leader>f', '<cmd>Telescope current_buffer_fuzzy_find<CR>')
map('n', '<leader>:', '<cmd>Telescope commands<CR>')
map('n', '<leader>bb', '<cmd>Telescope buffers<CR>')
map('n', '<leader>tz', '<cmd>TZAtaraxis<CR>')                           --ataraxis
map('n', '<leader>op', '<cmd>NvimTreeToggle<CR>')                      --nvimtree
map('n', '<leader>tw', '<cmd>set wrap!<CR>')                      --nvimtree
map('n', '<c-k>', '<cmd>wincmd k<CR>')                                 --ctrlhjkl to navigate splits
map('n', '<c-j>', '<cmd>wincmd j<CR>')
map('n', '<c-h>', '<cmd>wincmd h<CR>')
map('n', '<c-l>', '<cmd>wincmd l<CR>')

--disable builtin plugins
local disabled_built_ins = {
   "2html_plugin",
   "getscript",
   "getscriptPlugin",
   "gzip",
   "logipat",
   "netrw",
   "netrwPlugin",
   "netrwSettings",
   "netrwFileHandlers",
   "matchit",
   "tar",
   "tarPlugin",
   "rrhelper",
   "spellfile_plugin",
   "vimball",
   "vimballPlugin",
   "zip",
   "zipPlugin",
}

for _, plugin in pairs(disabled_built_ins) do
   g["loaded_" .. plugin] = 1
end

local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
  vim.cmd 'packadd packer.nvim'
end

return require('packer').startup(function()

   use {
      "wbthomason/packer.nvim",
   }

   use {
       "tweekmonster/startuptime.vim",
       cmd = "StartupTime"
   }

   use {
      "nvim-lua/plenary.nvim",
   }

   use {
      "shaunsingh/nord.nvim",
      after = "packer.nvim",
      config = function()
          vim.g.nord_borders = false
          vim.g.nord_contrast = false
          vim.g.nord_cursorline_transparent = true
          vim.g.nord_disable_background = false
          vim.cmd[[colorscheme nord]]
      end,
   }

   use {
      "kyazdani42/nvim-web-devicons",
      after = "nord.nvim",
      config = function()
          local present, icons = pcall(require, "nvim-web-devicons")
          if not present then
             return
          end

          local colors = {
            bg = "NONE",
            white = "#ECEFF4",
            fg = "#E5E9F0",
            yellow = "#EBCB8B",
            cyan = "#A3BE8C",
            darkblue = "#81A1C1",
            green = "#8FBCBB",
            orange = "#D08770",
            purple = "#B48EAD",
            magenta = "#BF616A",
            gray = "#616E88",
            blue = "#5E81AC",
            red = "#BF616A"
          }

          icons.setup {
             override = {
                c = {
                   icon = "",
                   color = colors.blue,
                   name = "c",
                },
                css = {
                   icon = "",
                   color = colors.blue,
                   name = "css",
                },
                deb = {
                   icon = "",
                   color = colors.cyan,
                   name = "deb",
                },
                Dockerfile = {
                   icon = "",
                   color = colors.cyan,
                   name = "Dockerfile",
                },
                html = {
                   icon = "",
                   color = colors.baby_pink,
                   name = "html",
                },
                jpeg = {
                   icon = "",
                   color = colors.dark_purple,
                   name = "jpeg",
                },
                jpg = {
                   icon = "",
                   color = colors.dark_purple,
                   name = "jpg",
                },
                js = {
                   icon = "",
                   color = colors.sun,
                   name = "js",
                },
                kt = {
                   icon = "󱈙",
                   color = colors.orange,
                   name = "kt",
                },
                lock = {
                   icon = "",
                   color = colors.red,
                   name = "lock",
                },
                lua = {
                   icon = "",
                   color = colors.blue,
                   name = "lua",
                },
                mp3 = {
                   icon = "",
                   color = colors.white,
                   name = "mp3",
                },
                mp4 = {
                   icon = "",
                   color = colors.white,
                   name = "mp4",
                },
                out = {
                   icon = "",
                   color = colors.white,
                   name = "out",
                },
                png = {
                   icon = "",
                   color = colors.dark_purple,
                   name = "png",
                },
                py = {
                   icon = "",
                   color = colors.cyan,
                   name = "py",
                },
                toml = {
                   icon = "",
                   color = colors.blue,
                   name = "toml",
                },
                ts = {
                   icon = "ﯤ",
                   color = colors.teal,
                   name = "ts",
                },
                rb = {
                   icon = "",
                   color = colors.pink,
                   name = "rb",
                },
                rpm = {
                   icon = "",
                   color = colors.orange,
                   name = "rpm",
                },
                vue = {
                   icon = "﵂",
                   color = colors.vibrant_green,
                   name = "vue",
                },
                xz = {
                   icon = "",
                   color = colors.sun,
                   name = "xz",
                },
                zip = {
                   icon = "",
                   color = colors.sun,
                   name = "zip",
                },
             },
          }
      end,
   }

   use {
      "glepnir/galaxyline.nvim",
      after = "nvim-web-devicons",
      config = function()
         local present1, gl = pcall(require, "galaxyline")
         local present2, condition = pcall(require, "galaxyline.condition")
         if not (present1 or present2) then
             return
         end

         local gls = gl.section

         gl.short_line_list = {" "}

         local colors = {
           bg = "NONE",
           white = "#ECEFF4",
           fg = "#E5E9F0",
           yellow = "#EBCB8B",
           cyan = "#A3BE8C",
           darkblue = "#81A1C1",
           green = "#8FBCBB",
           orange = "#D08770",
           purple = "#B48EAD",
           magenta = "#BF616A",
           gray = "#616E88",
           blue = "#5E81AC",
           red = "#BF616A"
         }

         gls.left[1] = {
             RainbowRed = {
             provider = function() return '▊ ' end,
             highlight = {colors.blue,colors.bg}
           },
         }
         gls.left[2] = {
             ViMode = {
                 provider = function()
                     -- auto change color according the vim mode
                     local mode_color = {
                         n = colors.cyan, i = colors.blue, v = colors.yellow,
                         [''] = colors.orange, V = colors.yellow,
                         c = colors.magenta, no = colors.green, s = colors.green,
                         S = colors.orange, [''] = colors.orange,
                         ic = colors.yellow, R = colors.magenta, Rv = colors.magenta,
                         cv = colors.red, ce = colors.red, r = colors.cyan,
                         rm = colors.cyan, ['r?'] = colors.cyan,
                         ['!'] = colors.red, t = colors.red
                     }
                     vim.api.nvim_command('hi GalaxyViMode guifg='..mode_color[vim.fn.mode()])
                     return ' λ '
                 end,
                 highlight = {colors.red,colors.bg,'bold'},
             },
         }
         gls.left[3] = {
           Space = {
             provider = function () return ' ' end
           }
         }
         gls.left[4] = {
             FileSize = {
                 provider = 'FileSize',
                 separator = '',
                 condition = condition.buffer_not_empty,
                 highlight = {colors.fg,colors.bg}
             },
         }
         gls.left[5] = {
           Space = {
             provider = function () return ' ' end
           }
         }
         gls.left[6] ={
             FileIcon = {
                 provider = 'FileIcon',
                 condition = condition.buffer_not_empty,
                 highlight = {require('galaxyline.provider_fileinfo').get_file_icon_color,colors.bg},
             },
         }
         gls.left[7] = {
             FileName = {
                 provider = {'FileName'},
                 separator = '',
                 condition = condition.buffer_not_empty,
                 highlight = {colors.green, colors.bg,'bold'}
             }

         }
         gls.left[8] = {
           Space = {
             provider = function () return ' ' end
           }
         }
         gls.left[9] = {
             LineInfo = {
                 provider = 'LineColumn',
                 separator = '',
                 separator_highlight = {'NONE',colors.bg},
                 highlight = {colors.fg,colors.bg},
             },
         }
         gls.left[10] = {
             PerCent = {
                 provider = 'LinePercent',
                 separator = '',
                 separator_highlight = {'NONE',colors.bg},
                 highlight = {colors.grey,colors.bg,'bold'},
             }
         }
         gls.left[11] = {
             DiagnosticError = {
                 provider = 'DiagnosticError',
                 icon = '  ',
                 highlight = {colors.red,colors.bg}
             }
         }
         gls.left[12] = {
             DiagnosticWarn = {
                 provider = 'DiagnosticWarn',
                 icon = '  ',
                 highlight = {colors.yellow,colors.bg},
             }
         }
         gls.left[13] = {
             DiagnosticHint = {
                 provider = 'DiagnosticHint',
                 icon = '  ',
                 highlight = {colors.cyan,colors.bg},
             }
         }
         gls.left[14] = {
             DiagnosticInfo = {
                 provider = 'DiagnosticInfo',
                 icon = '  ',
                 highlight = {colors.blue,colors.bg},
             }
         }
         gls.right[1] = {
             FileEncode = {
                 provider = 'FileEncode',
                 condition = condition.hide_in_width,
                 separator = '  ',
                 separator_highlight = {'NONE',colors.bg},
                 highlight = {colors.green,colors.bg,'bold'}
             }
         }

         gls.right[2] = {
             ShowLspClient = {
                 provider = 'GetLspClient',
                 condition = function ()
                   local tbl = {['dashboard'] = true,['']=true}
                   if tbl[vim.bo.filetype] then
                     return false
                   end
                   return true
                 end,
                 icon = '   ',
                 highlight = {colors.yellow,colors.bg,'bold'}
             }
         }
         gls.right[3] = {
             FileFormat = {
                 provider = 'FileFormat',
                 condition = condition.hide_in_width,
                 separator = '  ',
                 separator_highlight = {'NONE',colors.bg},
                 highlight = {colors.green,colors.bg,'bold'}
             }
         }
         gls.right[4] = {
             GitIcon = {
                 provider = function() return '  ' end,
                 condition = condition.check_git_workspace,
                 separator = ' ',
                 separator_highlight = {'NONE',colors.bg},
                 highlight = {colors.magenta,colors.bg,'bold'},
             }
         }
         gls.right[5] = {
             GitBranch = {
                 provider = 'GitBranch',
                 condition = condition.check_git_workspace,
                 highlight = {colors.magenta,colors.bg,'bold'},
             }
         }
         gls.right[6] = {
             DiffAdd = {
                 provider = 'DiffAdd',
                 condition = condition.hide_in_width,
                 icon = '    ',
                 highlight = {colors.green,colors.bg},
             }
         }
         gls.right[7] = {
             DiffModified = {
                 provider = 'DiffModified',
                 condition = condition.hide_in_width,
                 icon = ' 柳',
                 highlight = {colors.orange,colors.bg},
             }
         }
         gls.right[8] = {
             DiffRemove = {
                 provider = 'DiffRemove',
                 condition = condition.hide_in_width,
                 icon = '  ',
                 highlight = {colors.red,colors.bg},
             }
         }

         gls.short_line_left[1] = {
             BufferType = {
                 provider = 'FileTypeName',
                 separator = ' ',
                 separator_highlight = {'NONE',colors.bg},
                 highlight = {colors.blue,colors.bg,'bold'}
             }
         }
         gls.short_line_left[2] = {
             SFileName = {
                 provider =  'SFileName',
                 condition = condition.buffer_not_empty,
                 highlight = {colors.fg,colors.bg,'bold'}
             }
         }

         gls.short_line_right[1] = {
             BufferIcon = {
                 provider= 'BufferIcon',
                 highlight = {colors.fg,colors.bg}
             }
         }
      end,
   }

   use {
      "akinsho/bufferline.nvim",
      after = "nvim-web-devicons",
      config = function()
         local present, bufferline = pcall(require, "bufferline")
         if not present then
            return
         end

         -- function executed for top right close button in bufferline
         vim.cmd "function! Doom_bufferline_quitvim(a,b,c,d) \n qa \n endfunction"

         local colors = {
           bg = "NONE",
           black = "#2E3440",
           black2 = "#3b4252",
           white = "#ECEFF4",
           fg = "#E5E9F0",
           yellow = "#EBCB8B",
           cyan = "#A3BE8C",
           darkblue = "#81A1C1",
           green = "#8FBCBB",
           orange = "#D08770",
           purple = "#B48EAD",
           magenta = "#BF616A",
           gray = "#616E88",
           blue = "#5E81AC",
           red = "#BF616A"
         }

         bufferline.setup {
            options = {
               offsets = { { filetype = "NvimTree", text = "", padding = 1 } },
               buffer_close_icon = "",
               modified_icon = "",
               close_icon = "",
               show_close_icon = true,
               left_trunc_marker = "",
               right_trunc_marker = "",
               max_name_length = 14,
               max_prefix_length = 13,
               tab_size = 20,
               show_tab_indicators = true,
               enforce_regular_tabs = false,
               view = "multiwindow",
               show_buffer_close_icons = true,
               separator_style = "thin",
               always_show_bufferline = true,
               diagnostics = false, -- "or nvim_lsp"
               custom_filter = function(buf_number)
                  -- Func to filter out our managed/persistent split terms
                  local present_type, type = pcall(function()
                     return vim.api.nvim_buf_get_var(buf_number, "term_type")
                  end)

                  if present_type then
                     if type == "vert" then
                        return false
                     elseif type == "hori" then
                        return false
                     else
                        return true
                     end
                  else
                     return true
                  end
               end,
            },

            highlights = {
               background = {
                  guifg = colors.fg,
                  guibg = colors.black2,
               },

               -- buffers
               buffer_selected = {
                  guifg = colors.white,
                  guibg = colors.black,
                  gui = "bold",
               },
               buffer_visible = {
                  guifg = colors.gray,
                  guibg = colors.black2,
               },

               -- for diagnostics = "nvim_lsp"
               error = {
                  guifg = colors.gray,
                  guibg = colors.black2,
               },
               error_diagnostic = {
                  guifg = colors.gray,
                  guibg = colors.black2,
               },

               -- close buttons
               close_button = {
                  guifg = colors.gray,
                  guibg = colors.black2,
               },
               close_button_visible = {
                  guifg = colors.gray,
                  guibg = colors.black2,
               },
               close_button_selected = {
                  guifg = colors.red,
                  guibg = colors.black,
               },
               fill = {
                  guifg = colors.fg,
                  guibg = colors.black2,
               },
               indicator_selected = {
                  guifg = colors.black,
                  guibg = colors.black,
               },

               -- modified
               modified = {
                  guifg = colors.red,
                  guibg = colors.black2,
               },
               modified_visible = {
                  guifg = colors.red,
                  guibg = colors.black2,
               },
               modified_selected = {
                  guifg = colors.green,
                  guibg = colors.black,
               },

               -- separators
               separator = {
                  guifg = colors.black2,
                  guibg = colors.black2,
               },
               separator_visible = {
                  guifg = colors.black2,
                  guibg = colors.black2,
               },
               separator_selected = {
                  guifg = colors.black2,
                  guibg = colors.black2,
               },
               -- tabs
               tab = {
                  guifg = colors.fg,
                  guibg = colors.gray,
               },
               tab_selected = {
                  guifg = colors.black2,
                  guibg = colors.fg,
               },
               tab_close = {
                  guifg = colors.red,
                  guibg = colors.black,
               },
            },
         }
      end,
   }

   use {
      "lukas-reineke/indent-blankline.nvim",
      event = "BufRead",
      config = function()
         require("indent_blankline").setup {
               indentLine_enabled = 1,
               char = "▏",
               filetype_exclude = {
                  "help",
                  "terminal",
                  "dashboard",
                  "packer",
                  "lspinfo",
                  "TelescopePrompt",
                  "TelescopeResults",
               },
               buftype_exclude = { "terminal" },
               show_trailing_blankline_indent = false,
               show_first_indent_level = false,
            }
      end,
   }

   use {
      "norcalli/nvim-colorizer.lua",
      event = "BufRead",
      config = function()
         local present, colorizer = pcall(require, "colorizer")
         if present then
            colorizer.setup({ "*" }, {
               RGB = true, -- #RGB hex codes
               RRGGBB = true, -- #RRGGBB hex codes
               names = false, -- "Name" codes like Blue
               RRGGBBAA = false, -- #RRGGBBAA hex codes
               rgb_fn = false, -- CSS rgb() and rgba() functions
               hsl_fn = false, -- CSS hsl() and hsla() functions
               css = false, -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
               css_fn = false, -- Enable all CSS *functions*: rgb_fn, hsl_fn

               -- Available modes: foreground, background
               mode = "background", -- Set the display mode.
            })
            vim.cmd "ColorizerReloadAllBuffers"
         end
      end,
   }

   use {
      "nvim-treesitter/nvim-treesitter",
      event = "BufRead",
      config = function()
        local present, ts_config = pcall(require, "nvim-treesitter.configs")
        if not present then
           return
        end

        ts_config.setup {
          ensure_installed = "lua", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
          highlight = {
            enable = true,              -- false will disable the whole extension
            use_languagetree = true,
          },
        }
      end,
   }

   use {
       "p00f/nvim-ts-rainbow",
        after = "nvim-treesitter",
        config = function()
            require'nvim-treesitter.configs'.setup {
              rainbow = {
                enable = true,
                extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
                max_file_lines = nil, -- Do not enable for files with more than n lines, int
                -- colors = {}, -- table of hex strings
                -- termcolors = {} -- table of colour name strings
              }
            }
        end,
   }

   use {
      "lewis6991/gitsigns.nvim",
      config = function()
         local present, gitsigns = pcall(require, "gitsigns")
         if not present then
            return
         end

         gitsigns.setup {
            keymaps = {
               -- Default keymap options
               buffer = true,
               noremap = true,
               ["n ]c"] = { expr = true, "&diff ? ']c' : '<cmd>lua require\"gitsigns\".next_hunk()<CR>'" },
               ["n [c"] = { expr = true, "&diff ? '[c' : '<cmd>lua require\"gitsigns\".prev_hunk()<CR>'" },
               ["n <leader>hs"] = '<cmd>lua require"gitsigns".stage_hunk()<CR>',
               ["n <leader>hu"] = '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
               ["n <leader>hr"] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
               ["n <leader>hp"] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
               ["n <leader>hb"] = '<cmd>lua require"gitsigns".blame_line()<CR>',
            },
            numhl = false,

            sign_priority = 5,
            signs = {
               add = { hl = "DiffAdd", text = "│", numhl = "GitSignsAddNr" },
               change = { hl = "DiffChange", text = "│", numhl = "GitSignsChangeNr" },
               changedelete = { hl = "DiffChange", text = "~", numhl = "GitSignsChangeNr" },
               delete = { hl = "DiffDelete", text = "_", numhl = "GitSignsDeleteNr" },
               topdelete = { hl = "DiffDelete", text = "‾", numhl = "GitSignsDeleteNr" },
            },

            status_formatter = nil, -- Use default
            watch_index = {
               interval = 100,
            },
         }
      end,
   }

   use {
      "neovim/nvim-lspconfig",
      after = "nvim-lspinstall",
      config = function()
         local present1, lspconfig = pcall(require, "lspconfig")
         local present2, lspinstall = pcall(require, "lspinstall")

         if not (present1 or present2) then
            return
         end

         local function on_attach(_, bufnr)
            local function buf_set_keymap(...)
               vim.api.nvim_buf_set_keymap(bufnr, ...)
            end
            local function buf_set_option(...)
               vim.api.nvim_buf_set_option(bufnr, ...)
            end

            -- Enable completion triggered by <c-x><c-o>
            buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

            -- Mappings.
            local opts = { noremap = true, silent = true }

            -- See `:help vim.lsp.*` for documentation on any of the below functions
            buf_set_keymap("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
            buf_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
            buf_set_keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
            buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
            buf_set_keymap("n", "gk", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
            buf_set_keymap("n", "<space>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
            buf_set_keymap("n", "<space>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
            buf_set_keymap("n", "<space>wl", "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", opts)
            buf_set_keymap("n", "<space>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
            buf_set_keymap("n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
            buf_set_keymap("n", "<space>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
            buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
            buf_set_keymap("n", "<space>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
            buf_set_keymap("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
            buf_set_keymap("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
            buf_set_keymap("n", "<space>q", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", opts)
            buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
            buf_set_keymap("v", "<space>ca", "<cmd>lua vim.lsp.buf.range_code_action()<CR>", opts)
         end

         local capabilities = vim.lsp.protocol.make_client_capabilities()
         capabilities.textDocument.completion.completionItem.documentationFormat = { "markdown", "plaintext" }
         capabilities.textDocument.completion.completionItem.snippetSupport = true
         capabilities.textDocument.completion.completionItem.preselectSupport = true
         capabilities.textDocument.completion.completionItem.insertReplaceSupport = true
         capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
         capabilities.textDocument.completion.completionItem.deprecatedSupport = true
         capabilities.textDocument.completion.completionItem.commitCharactersSupport = true
         capabilities.textDocument.completion.completionItem.tagSupport = { valueSet = { 1 } }
         capabilities.textDocument.completion.completionItem.resolveSupport = {
            properties = {
               "documentation",
               "detail",
               "additionalTextEdits",
            },
         }

         local function setup_servers()
            lspinstall.setup()
            local servers = lspinstall.installed_servers()

            for _, lang in pairs(servers) do
               if lang ~= "lua" then
                  lspconfig[lang].setup {
                     on_attach = on_attach,
                     capabilities = capabilities,
                     flags = {
                        debounce_text_changes = 500,
                     },
                     -- root_dir = vim.loop.cwd,
                  }
               elseif lang == "lua" then
                  lspconfig[lang].setup {
                     on_attach = on_attach,
                     capabilities = capabilities,
                     flags = {
                        debounce_text_changes = 500,
                     },
                     settings = {
                        Lua = {
                           diagnostics = {
                              globals = { "vim" },
                           },
                           workspace = {
                              library = {
                                 [vim.fn.expand "$VIMRUNTIME/lua"] = true,
                                 [vim.fn.expand "$VIMRUNTIME/lua/vim/lsp"] = true,
                              },
                              maxPreload = 100000,
                              preloadFileSize = 10000,
                           },
                           telemetry = {
                              enable = false,
                           },
                        },
                     },
                  }
               end
            end
         end

         setup_servers()

         -- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
         lspinstall.post_install_hook = function()
            setup_servers() -- reload installed servers
            vim.cmd "bufdo e"
         end

         -- replace the default lsp diagnostic symbols
         local function lspSymbol(name, icon)
            vim.fn.sign_define("LspDiagnosticsSign" .. name, { text = icon, numhl = "LspDiagnosticsDefaul" .. name })
         end

         lspSymbol("Error", "")
         lspSymbol("Information", "")
         lspSymbol("Hint", "")
         lspSymbol("Warning", "")

         vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
            virtual_text = {
               prefix = "",
               spacing = 0,
            },
            signs = true,
            underline = true,
            update_in_insert = false, -- update diagnostics insert mode
         })
         vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
            border = "single",
         })
         vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
            border = "single",
         })

         -- suppress error messages from lang servers
         vim.notify = function(msg, log_level, _opts)
            if msg:match "exit code" then
               return
            end
            if log_level == vim.log.levels.ERROR then
               vim.api.nvim_err_writeln(msg)
            else
               vim.api.nvim_echo({ { msg } }, true, {})
            end
         end
      end,
   }

   use {
      "kabouzeid/nvim-lspinstall",
   }

   use {
      "ray-x/lsp_signature.nvim",
      after = "nvim-lspconfig",
      config = function()
         local present, lspsignature = pcall(require, "lsp_signature")
            if present then
               lspsignature.setup {
                  bind = true,
                  doc_lines = 2,
                  floating_window = true,
                  fix_pos = true,
                  hint_enable = true,
                  hint_prefix = " ",
                  hint_scheme = "String",
                  hi_parameter = "Search",
                  max_height = 22,
                  max_width = 120, -- max_width of signature floating_window, line will be wrapped if exceed max_width
                  handler_opts = {
                     border = "single", -- double, single, shadow, none
                  },
                  zindex = 200, -- by default it will be on top of all floating windows, set to 50 send it to bottom
                  padding = "", -- character to pad on left and right of signature can be ' ', or '|'  etc
               }
            end
      end,
   }

   use {
      "rafamadriz/friendly-snippets",
      event = "InsertEnter",
   }

   use {
      "hrsh7th/nvim-cmp",
      after = "friendly-snippets",
      config = function()
         local present, cmp = pcall(require, "cmp")

         if not present then
            return
         end

         vim.opt.completeopt = "menuone,noselect"

         -- nvim-cmp setup
         cmp.setup {
            snippet = {
               expand = function(args)
                  require("luasnip").lsp_expand(args.body)
               end,
            },
            formatting = {
               format = function(entry, vim_item)
                  vim_item.menu = ({
                     nvim_lsp = "[LSP]",
                     nvim_lua = "[Lua]",
                     buffer = "[BUF]",
                  })[entry.source.name]

                  return vim_item
               end,
            },
            mapping = {
               ["<C-p>"] = cmp.mapping.select_prev_item(),
               ["<C-n>"] = cmp.mapping.select_next_item(),
               ["<C-d>"] = cmp.mapping.scroll_docs(-4),
               ["<C-f>"] = cmp.mapping.scroll_docs(4),
               ["<C-Space>"] = cmp.mapping.complete(),
               ["<C-e>"] = cmp.mapping.close(),
               ["<CR>"] = cmp.mapping.confirm {
                  behavior = cmp.ConfirmBehavior.Replace,
                  select = true,
               },
               ["<Tab>"] = function(fallback)
                  if vim.fn.pumvisible() == 1 then
                     vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<C-n>", true, true, true), "n")
                  elseif require("luasnip").expand_or_jumpable() then
                     vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<Plug>luasnip-expand-or-jump", true, true, true), "")
                  else
                     fallback()
                  end
               end,
               ["<S-Tab>"] = function(fallback)
                  if vim.fn.pumvisible() == 1 then
                     vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<C-p>", true, true, true), "n")
                  elseif require("luasnip").jumpable(-1) then
                     vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<Plug>luasnip-jump-prev", true, true, true), "")
                  else
                     fallback()
                  end
               end,
            },
            sources = {
               { name = "nvim_lsp" },
               { name = "luasnip" },
               { name = "buffer" },
               { name = "nvim_lua" },
               { name = "orgmode" },
            },
         }
      end,
   }

   use {
      "L3MON4D3/LuaSnip",
      wants = "friendly-snippets",
      after = "nvim-cmp",
      config = function()
         local present, luasnip = pcall(require, "luasnip")
         if not present then
            return
         end

         luasnip.config.set_config {
            history = true,
            updateevents = "TextChanged,TextChangedI",
         }
         require("luasnip/loaders/from_vscode").load()
      end,
   }

   use {
      "saadparwaiz1/cmp_luasnip",
      after = "LuaSnip",
   }

   use {
      "hrsh7th/cmp-nvim-lua",
      after = "cmp_luasnip",
   }

   use {
      "hrsh7th/cmp-nvim-lsp",
      after = "cmp-nvim-lua",
   }

   use {
      "hrsh7th/cmp-buffer",
      after = "cmp-nvim-lsp",
   }

   use {
      "windwp/nvim-autopairs",
      after = "nvim-cmp",
      config = function()
         local present1, autopairs = pcall(require, "nvim-autopairs")
            local present2, autopairs_completion = pcall(require, "nvim-autopairs.completion.cmp")

            if not (present1 or present2) then
               return
            end

            autopairs.setup()
            autopairs_completion.setup {
               map_complete = true, -- insert () func completion
               map_cr = true,
            }
      end,
   }

   use {
      "kyazdani42/nvim-tree.lua",
      cmd = { "NvimTreeToggle", "NvimTreeFocus" },
      config = function()
         local present, tree_c = pcall(require, "nvim-tree.config")
         if not present then
            return
         end

         local colors = {
           bg = "NONE",
           white = "#ECEFF4",
           fg = "#E5E9F0",
           yellow = "#EBCB8B",
           cyan = "#A3BE8C",
           darkblue = "#81A1C1",
           green = "#8FBCBB",
           orange = "#D08770",
           purple = "#B48EAD",
           magenta = "#BF616A",
           gray = "#616E88",
           blue = "#5E81AC",
           red = "#BF616A"
         }

         local tree_cb = tree_c.nvim_tree_callback
         local g = vim.g

         vim.o.termguicolors = true

         g.nvim_tree_add_trailing = 0 -- append a trailing slash to folder names
         g.nvim_tree_allow_resize = 1
         g.nvim_tree_auto_close = 0 -- closes tree when it's the last window
         g.nvim_tree_auto_ignore_ft = { "dashboard" } -- don't open tree on specific fiypes.
         g.nvim_tree_auto_open = 0
         g.nvim_tree_disable_netrw = 1
         g.nvim_tree_follow = 1
         g.nvim_tree_git_hl = 1
         g.nvim_tree_gitignore = 1
         g.nvim_tree_hide_dotfiles = 0
         g.nvim_tree_highlight_opened_files = 0
         g.nvim_tree_hijack_netrw = 0
         g.nvim_tree_indent_markers = 1
         g.nvim_tree_ignore = { ".git", "node_modules", ".cache" }
         g.nvim_tree_quit_on_open = 0 -- closes tree when file's opened
         g.nvim_tree_root_folder_modifier = table.concat { ":t:gs?$?/..", string.rep(" ", 1000), "?:gs?^??" }
         g.nvim_tree_side = "left"
         g.nvim_tree_tab_open = 0
         g.nvim_tree_update_cwd = 1
         g.nvim_tree_width = 25
         g.nvim_tree_lsp_diagnostics = 0

         g.nvim_tree_show_icons = {
            folders = 1,
            -- folder_arrows= 1
            files = 1,
            git = 1,
         }

         g.nvim_tree_icons = {
            default = "",
            symlink = "",
            git = {
               deleted = "",
               ignored = "◌",
               renamed = "➜",
               staged = "✓",
               unmerged = "",
               unstaged = "✗",
               untracked = "★",
            },
            folder = {
               -- disable indent_markers option to get arrows working or if you want both arrows and indent then just add the arrow icons in front            ofthe default and opened folders below!
               -- arrow_open = "",
               -- arrow_closed = "",
               default = "",
               empty = "", -- 
               empty_open = "",
               open = "",
               symlink = "",
               symlink_open = "",
            },
         }

         g.nvim_tree_bindings = {
            { key = { "<CR>", "o", "<2-LeftMouse>" }, cb = tree_cb "edit" },
            { key = { "<2-RightMouse>", "<C-]>" }, cb = tree_cb "cd" },
            { key = "<C-v>", cb = tree_cb "vsplit" },
            { key = "<C-x>", cb = tree_cb "split" },
            { key = "<C-t>", cb = tree_cb "tabnew" },
            { key = "<", cb = tree_cb "prev_sibling" },
            { key = ">", cb = tree_cb "next_sibling" },
            { key = "P", cb = tree_cb "parent_node" },
            { key = "<BS>", cb = tree_cb "close_node" },
            { key = "<S-CR>", cb = tree_cb "close_node" },
            { key = "<Tab>", cb = tree_cb "preview" },
            { key = "K", cb = tree_cb "first_sibling" },
            { key = "J", cb = tree_cb "last_sibling" },
            { key = "I", cb = tree_cb "toggle_ignored" },
            { key = "H", cb = tree_cb "toggle_dotfiles" },
            { key = "R", cb = tree_cb "refresh" },
            { key = "a", cb = tree_cb "create" },
            { key = "d", cb = tree_cb "remove" },
            { key = "r", cb = tree_cb "rename" },
            { key = "<C->", cb = tree_cb "full_rename" },
            { key = "x", cb = tree_cb "cut" },
            { key = "c", cb = tree_cb "copy" },
            { key = "p", cb = tree_cb "paste" },
            { key = "y", cb = tree_cb "copy_name" },
            { key = "Y", cb = tree_cb "copy_path" },
            { key = "gy", cb = tree_cb "copy_absolute_path" },
            { key = "[c", cb = tree_cb "prev_git_item" },
            { key = "}c", cb = tree_cb "next_git_item" },
            { key = "-", cb = tree_cb "dir_up" },
            { key = "q", cb = tree_cb "close" },
            { key = "g?", cb = tree_cb "toggle_help" },
         }
      end,
   }

   use {
      "nvim-telescope/telescope.nvim",
      cmd = "Telescope",
      requires = {
         {
            "nvim-telescope/telescope-fzf-native.nvim",
            run = "make",
         },
      },
      config = function()
         local present, telescope = pcall(require, "telescope")
         if not present then
            return
         end

         telescope.setup {
            defaults = {
               vimgrep_arguments = {
                  "rg",
                  "--color=never",
                  "--no-heading",
                  "--with-filename",
                  "--line-number",
                  "--column",
                  "--smart-case",
               },
               prompt_prefix = " λ ",
               selection_caret = " > ",
               entry_prefix = "  ",
               initial_mode = "insert",
               selection_strategy = "reset",
               sorting_strategy = "ascending",
               layout_strategy = "horizontal",
               layout_config = {
                  horizontal = {
                     prompt_position = "top",
                     preview_width = 0.55,
                     results_width = 0.8,
                  },
                  vertical = {
                     mirror = false,
                  },
                  width = 0.87,
                  height = 0.80,
                  preview_cutoff = 120,
               },
               file_sorter = require("telescope.sorters").get_fuzzy_file,
               file_ignore_patterns = {},
               generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
               path_display = { "absolute" },
               winblend = 0,
               border = {},
               borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
               color_devicons = true,
               use_less = true,
               set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
               file_previewer = require("telescope.previewers").vim_buffer_cat.new,
               grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
               qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
               -- Developer configurations: Not meant for general override
               buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
            },
            extensions = {
               fzf = {
                  fuzzy = true, -- false will only do exact matching
                  override_generic_sorter = false, -- override the generic sorter
                  override_file_sorter = true, -- override the file sorter
                  case_mode = "smart_case", -- or "ignore_case" or "respect_case"
                  -- the default case_mode is "smart_case"
               },
            },
         }

         local extensions = { "themes", "terms", "fzf" }
         local packer_repos = [["extensions", "telescope-fzf-native.nvim"]]

         pcall(function()
            for _, ext in ipairs(extensions) do
               telescope.load_extension(ext)
            end
         end)
      end,
   }

   use {
      "Pocco81/TrueZen.nvim",
      cmd = {
         "TZAtaraxis",
         "TZMinimalist",
         "TZFocus",
      },
      config = function()
         local present, true_zen = pcall(require, "true-zen")
         if not present then
            return
         end

         true_zen.setup {
            ui = {
               bottom = {
                  cmdheight = 1,
                  laststatus = 0,
                  ruler = false,
                  showmode = false,
                  showcmd = false,
               },
               top = {
                  showtabline = 0,
               },
               left = {
                  number = false,
                  relativenumber = false,
                  signcolumn = "no",
               },
            },
            modes = {
               ataraxis = {
                  left_padding = 32,
                  right_padding = 32,
                  top_padding = 1,
                  bottom_padding = 1,
                  ideal_writing_area_width = { 0 },
                  auto_padding = false,
                  keep_default_fold_fillchars = false,
                  custome_bg = "",
                  bg_configuration = true,
                  affected_higroups = {
                     NonText = {},
                     FoldColumn = {},
                     ColorColumn = {},
                     VertSplit = {},
                     StatusLine = {},
                     StatusLineNC = {},
                     SignColumn = {},
                  },
               },
               focus = {
                  margin_of_error = 5,
                  focus_method = "experimental",
               },
            },
            integrations = {
         	vim_gitgutter = false,
         	galaxyline = false,
         	tmux = false,
         	gitsigns = true,
         	nvim_bufferline = true,
         	limelight = false,
         	twilight = true,
         	vim_airline = false,
         	vim_powerline = false,
         	vim_signify = false,
         	express_line = false,
         	lualine = false,
         	lightline = false,
         	feline = false
            },
            misc = {
               on_off_commands = false,
               ui_elements_commands = false,
               cursor_by_mode = false,
            },
         }
      end,
   }

   use {
       "folke/twilight.nvim",
       cmd = {
           "Twilight",
           "TwilightEnable"
       },
       config = function()
           require("twilight").setup {}
       end
   }

   use {
       "phaazon/hop.nvim",
       cmd = {
           "HopWord",
           "HopLine",
           "HopChar1",
           "HopChar2",
           "HopPattern"
       },
       as = 'hop',
       config = function()
           -- you can configure Hop the way you like here; see :h hop-config
           require'hop'.setup()
       end
   }

   use {'kristijanhusak/orgmode.nvim',
     ft = {'org'},
     config = function()
           require('orgmode').setup({
               org_highlight_latex_and_related = 'entities',
               org_agenda_files = '~/org/*',
               org_default_notes_file = '~/org/notes.org',
               org_hide_leading_stars = true,
               org_hide_emphasis_markers = true,
               mappings = {
                 global = {
                   org_agenda = '<Leader>oa',
                   org_capture = '<Leader>oc',
                 },
                 agenda = {
                   org_agenda_later = 'f',
                   org_agenda_earlier = 'b',
                   org_agenda_goto_today = '.',
                   org_agenda_day_view = 'vd',
                   org_agenda_week_view = 'vw',
                   org_agenda_month_view = 'vm',
                   org_agenda_year_view = 'vy',
                   org_agenda_quit = 'q',
                   org_agenda_switch_to = '<CR>',
                   org_agenda_goto = { '<TAB>' },
                   org_agenda_goto_date = 'J',
                   org_agenda_redo = 'r',
                   org_agenda_todo = 't',
                   org_agenda_show_help = '?',
                 },
                 capture = {
                   org_capture_finalize = '<C-c>',
                   org_capture_refile = '<Leader>or',
                   org_capture_kill = '<Leader>ok',
                   org_capture_show_help = '?',
                 },
                 org = {
                   org_increase_date = '<C-a>',
                   org_decrease_date = '<C-x>',
                   org_toggle_checkbox = '<C-Space>',
                   org_open_at_point = '<Leader>oo',
                   org_cycle = '<TAB>',
                   org_global_cycle = '<S-TAB>',
                   org_archive_subtree = '<Leader>o$',
                   org_set_tags_command = '<Leader>ot',
                   org_toggle_archive_tag = '<Leader>oA',
                   org_do_promote = '<<',
                   org_do_demote = '>>',
                   org_promote_subtree = '<s',
                   org_demote_subtree = '>s',
                   org_meta_return = '<Leader><CR>', -- Add headling, item or row
                   org_insert_heading_respect_content = '<Leader>oih', -- Add new headling after current heading block with same level
                   org_insert_todo_heading = '<Leader>oiT', -- Add new todo headling right after current heading with same level
                   org_insert_todo_heading_respect_content = '<Leader>oit', -- Add new todo headling after current heading block on same level
                   org_move_subtree_up = '<Leader>oK',
                   org_move_subtree_down = '<Leader>oJ',
                   org_export = '<Leader>oe',
                   org_next_visible_heading = '}',
                   org_previous_visible_heading = '{',
                   org_forward_heading_same_level = ']]',
                   org_backward_heading_same_level = '[[',
                 },
               },
           })
     end
   }
end)
