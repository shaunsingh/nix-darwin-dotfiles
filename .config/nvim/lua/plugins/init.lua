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
      "ggandor/lightspeed.nvim",
      event = "BufRead",
   }

end)
