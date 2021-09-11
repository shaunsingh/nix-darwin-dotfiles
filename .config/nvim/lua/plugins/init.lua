local present, packer = pcall(require, "plugins.packerInit")

if not present then
   return false
end

local use = packer.use

return packer.startup(function()
   use {
      "nvim-lua/plenary.nvim",
   }

   use {
      "wbthomason/packer.nvim",
      event = "VimEnter",
   }

   use {
      "shaunsingh/nord.nvim",
      after = "packer.nvim",
      config = function()
         require("colors").init()
      end,
   }

   use {
      "kyazdani42/nvim-web-devicons",
      after = "nord.nvim",
      config = function()
         require "plugins.configs.icons"
      end,
   }

   use {
      "glepnir/galaxyline.nvim",
      after = "nvim-web-devicons",
      config = function()
         require "plugins.configs.statusline"
      end,
   }

   use {
      "akinsho/bufferline.nvim",
      after = "nvim-web-devicons",
      config = function()
         require "plugins.configs.bufferline"
      end,
      setup = function()
         require("core.mappings").bufferline()
      end,
   }

   use {
      "lukas-reineke/indent-blankline.nvim",
      event = "BufRead",
      config = function()
         require("plugins.configs.others").blankline()
      end,
   }

   use {
      "norcalli/nvim-colorizer.lua",
      event = "BufRead",
      config = function()
         require("plugins.configs.others").colorizer()
      end,
   }

   use {
      "nvim-treesitter/nvim-treesitter",
      event = "BufRead",
      config = function()
         require "plugins.configs.treesitter"
      end,
   }

   -- git stuff
   use {
      "lewis6991/gitsigns.nvim",
      opt = true,
      config = function()
         require "plugins.configs.gitsigns"
      end,
      setup = function()
         require("core.utils").packer_lazy_load "gitsigns.nvim"
      end,
   }

   -- lsp stuff
   use {
      "kabouzeid/nvim-lspinstall",
      opt = true,
      setup = function()
         require("core.utils").packer_lazy_load "nvim-lspinstall"
         -- reload the current file so lsp actually starts for it
         vim.defer_fn(function()
            vim.cmd "silent! e %"
         end, 0)
      end,
   }

   use {
      "neovim/nvim-lspconfig",
      after = "nvim-lspinstall",
      config = function()
         require "plugins.configs.lspconfig"
      end,
   }

   use {
      "ray-x/lsp_signature.nvim",
      after = "nvim-lspconfig",
      config = function()
         require("plugins.configs.others").signature()
      end,
   }

   use {
      "jdhao/better-escape.vim",
      event = "InsertEnter",
      config = function()
         require("plugins.configs.others").better_escape()
      end,
      setup = function()
         require("core.mappings").better_escape()
      end,
   }

   -- load luasnips + cmp related in insert mode only

   use {
      "rafamadriz/friendly-snippets",
      event = "InsertEnter",
   }

   use {
      "hrsh7th/nvim-cmp",
      after = "friendly-snippets",
      config = function()
         require "plugins.configs.cmp"
      end,
   }

   use {
      "L3MON4D3/LuaSnip",
      wants = "friendly-snippets",
      after = "nvim-cmp",
      config = function()
         require("plugins.configs.others").luasnip()
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

   -- misc plugins
   use {
      "windwp/nvim-autopairs",
      after = "nvim-cmp",
      config = function()
         require("plugins.configs.others").autopairs()
      end,
   }

   use {
      "glepnir/dashboard-nvim",
      config = function()
         require "plugins.configs.dashboard"
      end,
   }

   use {
      "sbdchd/neoformat",
      cmd = "Neoformat",
      setup = function()
         require("core.mappings").neoformat()
      end,
   }

   use {
      "terrortylor/nvim-comment",
      cmd = "CommentToggle",
      config = function()
         require("plugins.configs.others").comment()
      end,
      setup = function()
         require("core.mappings").comment()
      end,
   }

   -- file managing , picker etc
   use {
      "kyazdani42/nvim-tree.lua",
      cmd = { "NvimTreeToggle", "NvimTreeFocus" },
      config = function()
         require "plugins.configs.nvimtree"
      end,
      setup = function()
         require("core.mappings").nvimtree()
      end,
   }

   use {
      "nvim-telescope/telescope.nvim",
      cmd = "Telescope",
      -- because cheatsheet is not activated by a teleacope command
      module = "cheatsheet",
      requires = {
         {
            "sudormrfbin/cheatsheet.nvim",
            after = "telescope.nvim",
            config = function()
               require "plugins.configs.mapsheet"
            end,
            setup = function()
               require("core.mappings").mapsheet()
            end,
         },
         {
            "nvim-telescope/telescope-fzf-native.nvim",
            run = "make",
         },
         {
            "nvim-telescope/telescope-media-files.nvim",
            setup = function()
               require("core.mappings").telescope_media()
            end,
         },
      },
      config = function()
         require "plugins.configs.telescope"
      end,
      setup = function()
         require("core.mappings").telescope()
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
         require "plugins.configs.zenmode"
      end,
      setup = function()
         require("core.mappings").truezen()
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
      "ggandor/lightspeed.nvim",
      event = "BufRead",
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
