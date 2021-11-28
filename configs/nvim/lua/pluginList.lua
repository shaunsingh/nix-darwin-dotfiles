local present, packer = pcall(require, "packerInit")

if present then
   packer = require "packer"
else
   return false
end

local use = packer.use
return packer.startup(function()
   -- Have packer manage itself
   use {
      "wbthomason/packer.nvim",
      event = "VimEnter",
   }

   use {
      "VonHeikemen/fine-cmdline.nvim",
      -- cmd = { "lua require('fine-cmdline').open()" },
      requires = {
         "MunifTanjim/nui.nvim",
      },
      config = function()
         require("plugins.others").fineCmdline()
      end,
   }

   use {
     'VonHeikemen/searchbox.nvim',
     -- cmd = { "lua require('searchbox').incsearch()" },
     requires = {
       {'MunifTanjim/nui.nvim'}
     },
     config = function()
        require("plugins.others").searchbox()
     end,
   }

   -- Startup optimizations
   use {
      "nathom/filetype.nvim",
   }

   use {
      "lewis6991/impatient.nvim",
   }

   use {
      "tweekmonster/startuptime.vim",
      cmd = "StartupTime",
   }

   -- Use fancy plugin for JK escape
   use {
      "max397574/better-escape.nvim",
      event = "InsertEnter",
      config = function()
         require("better_escape").setup {
            mapping = { "jk", "jj" },
            clear_empty_lines = true,
            keys = "<Esc>",
         }
      end,
   }

   use {
     "folke/which-key.nvim",
     after = "nord.nvim",
     config = function()
       require("which-key").setup {
         -- your configuration comes here
         -- or leave it empty to use the default settings
         -- refer to the configuration section below
       }
     end
   }

   -- Theme <3 and UI
   use {
      --"shaunsingh/nord.nvim",
      "Clutch-Squad-10669/nord.nvim",
      after = "packer.nvim",
      config = function()
         require("nord").set()
      end,
   }

   use {
      "kyazdani42/nvim-web-devicons",
      after = "nord.nvim",
   }

   use {
      "NTBBloodbath/galaxyline.nvim",
      after = "nvim-web-devicons",
      config = function()
         require "plugins.statusline"
      end,
   }

   use {
      "akinsho/bufferline.nvim",
      after = "nvim-web-devicons",
      config = function()
         require "plugins.bufferline"
      end,
   }

   use {
      "lukas-reineke/indent-blankline.nvim",
      after = "nord.nvim",
      config = function()
         require("plugins.others").blankline()
      end,
   }

   use {
      "norcalli/nvim-colorizer.lua",
      cmd = "ColorizerToggle",
      config = function()
         require("plugins.others").colorizer()
      end,
   }

   use {
      "nvim-treesitter/nvim-treesitter",
      after = "nord.nvim",
      config = function()
         require "plugins.treesitter"
      end,
   }

   use {
      "nvim-treesitter/playground",
      cmd = "TSPlayground",
   }

   use {
      "p00f/nvim-ts-rainbow",
      after = "nvim-treesitter",
   }

   use {
      "lewis6991/gitsigns.nvim",
      after = "nord.nvim",
   }

   use {
      "kyazdani42/nvim-tree.lua",
      cmd = { "NvimTreeToggle", "NvimTreeFocus" },
      config = function()
         require "plugins.nvimtree"
      end,
   }

   -- LSP (and copilot
   use {
      "github/copilot.vim",
      event = "InsertEnter",
   }

   use {
      "neovim/nvim-lspconfig",
      after = "nvim-lsp-installer",
      config = function()
         require "plugins.lspconfig"
      end,
   }

   use {
      "williamboman/nvim-lsp-installer",
      event = "InsertEnter",
   }

   use {
      "ray-x/lsp_signature.nvim",
      after = "nvim-lspconfig",
      config = function()
         require("plugins.others").signature()
      end,
   }

   use {
      "rafamadriz/friendly-snippets",
      event = "InsertEnter",
   }

   use {
      'numToStr/Comment.nvim',
      after = "friendly-snippets",
      config = function()
         require('plugins.others').comment()
      end
   }

   use {
      "hrsh7th/nvim-cmp",
      after = "friendly-snippets",
      config = function()
         require "plugins.cmp"
      end,
   }

   use {
      "L3MON4D3/LuaSnip",
      wants = "friendly-snippets",
      after = "nvim-cmp",
      config = function()
         require("plugins.others").luasnip()
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
      "lukas-reineke/cmp-rg",
      after = "cmp-nvim-lsp",
   }

   use {
      "ray-x/cmp-treesitter",
      after = "cmp-nvim-lsp",
   }

   use {
      "hrsh7th/cmp-path",
      after = "cmp-rg",
   }

   use {
      "nvim-telescope/telescope.nvim",
      cmd = "Telescope",
      requires = {
         {
            "nvim-telescope/telescope-fzf-native.nvim",
            "nvim-lua/plenary.nvim",
            run = "make",
         },
      },
      config = function()
         require "plugins.telescope"
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
         require "plugins.zenmode"
      end,
   }

   use {
      "folke/twilight.nvim",
      cmd = {
         "Twilight",
         "TwilightEnable",
      },
      config = function()
         require("twilight").setup {}
      end,
   }

   use {
      "phaazon/hop.nvim",
      cmd = {
         "HopWord",
         "HopLine",
         "HopChar1",
         "HopChar2",
         "HopPattern",
      },
      as = "hop",
      config = function()
         require("hop").setup()
      end,
   }

   use {
      "sindrets/diffview.nvim",
      after = "neogit",
   }

   use {
      "TimUntersberger/neogit",
      cmd = {
         "Neogit",
         "Neogit commit",
      },
      config = function()
         require "plugins.neogit"
      end,
   }

   use {
      "nvim-neorg/neorg",
      branch = "unstable",
      ft = "norg",
      config = function()
         require "plugins.neorg"
      end,
   }

   use {
      "nvim-neorg/neorg-telescope",
      ft = "norg",
   }
end)
