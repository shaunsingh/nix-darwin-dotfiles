local present, packer = pcall(require, "packerInit")

if present then
   packer = require "packer"
else
   return false
end

local use = packer.use
return packer.startup(function()
   use {
      "wbthomason/packer.nvim",
      event = "VimEnter",
   }

   use {
      "nvim-lua/plenary.nvim",
   }

   use {
      "dstein64/vim-startuptime",
      cmd = "StartupTime",
   }
   use {
      "max397574/better-escape.nvim",
      event = "InsertEnter",
      config = function()
        require("better_escape").setup {
          mapping = {"jk", "jj"}, 
          timeout = vim.o.timeoutlen,
          clear_empty_lines = true,
          keys = "<Esc>",
        }
      end,
    }

   use {
      "shaunsingh/nord.nvim",
      after = "packer.nvim",
      config = function()
         vim.g.nord_borders = false
         vim.g.nord_contrast = false
         vim.g.nord_cursorline_transparent = true
         vim.g.nord_disable_background = true
         vim.cmd [[colorscheme nord]]
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
      event = "BufRead",
      config = function()
         require("plugins.others").blankline()
      end,
   }

   use {
      "norcalli/nvim-colorizer.lua",
      event = "BufRead",
      config = function()
         require("plugins.others").colorizer()
      end,
   }

   use {
      "nvim-treesitter/nvim-treesitter",
      event = "BufRead",
      config = function()
         require "plugins.treesitter"
      end,
   }

   use {
      "p00f/nvim-ts-rainbow",
      after = "nvim-treesitter",
   }

   use {
      "lewis6991/gitsigns.nvim",
      config = function()
         require "plugins.gitsigns"
      end,
   }

   use {
      "neovim/nvim-lspconfig",
      after = "nvim-lspinstall",
      config = function()
         require "plugins.lspconfig"
      end,
   }

   use {
      "kabouzeid/nvim-lspinstall",
   }

   use {
      "weilbith/nvim-code-action-menu",
      cmd = 'CodeActionMenu',
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
      "hrsh7th/nvim-cmp",
      module = "cmp",
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
      module = "cmp_nvim_lsp",
      after = "nvim-lspconfig",
   }

   use {
      "hrsh7th/cmp-buffer",
      after = "cmp-nvim-lsp",
   }

   use {
      "windwp/nvim-autopairs",
      after = "nvim-cmp",
   }

   use {
      "kyazdani42/nvim-tree.lua",
      cmd = { "NvimTreeToggle", "NvimTreeFocus" },
      config = function()
         require "plugins.nvimtree"
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
      "kristijanhusak/orgmode.nvim",
      ft = { "org" },
      config = function()
         require("plugins.others").orgmode()
      end,
   }

   use {
      "TimUntersberger/neogit",
      cmd = {
         "Neogit",
         "Neogit commit",
      },
      requires = { 
         "nvim-lua/plenary.nvim",
         "sindrets/diffview.nvim",
      },
      config = function()
         require "plugins.neogit"
      end,
   }

end)
