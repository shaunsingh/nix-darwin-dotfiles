--packer
local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
  execute 'packadd packer.nvim'
end

vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'
  --lualine/nvim-web-devicons
  use {
    'hoob3rt/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
  }
  --barbar
  use 'romgrk/barbar.nvim'
  --my moonlight theme
  use 'shaunsingh/material.nvim'
  --nvim-tree
  use 'kyazdani42/nvim-tree.lua'
  --neogit + gitsigns
  use { 'TimUntersberger/neogit', requires = 'nvim-lua/plenary.nvim' }
  use { 'lewis6991/gitsigns.nvim', requires = 'nvim-lua/plenary.nvim'}
  --dashboard-nvim
  use 'glepnir/dashboard-nvim'
  --dashboard
  use 'kdav5758/TrueZen.nvim'
  --treesitter
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
  --indentlines
  use 'Yggdroot/indentLine' 
  use 'lukas-reineke/indent-blankline.nvim'
  --vim-visual-multi
  use 'mg979/vim-visual-multi'
  --linting/lsp
  use 'hrsh7th/nvim-compe' 
  use 'onsails/lspkind-nvim' 
  use 'neovim/nvim-lspconfig' 
  use 'folke/lsp-trouble.nvim' 
  use 'glepnir/lspsaga.nvim'   
  --snpipets 
  use 'hrsh7th/vim-vsnip'   
  use 'hrsh7th/vim-vsnip-integ'   
  use 'rafamadriz/friendly-snippets' 
  --hop
  use 'phaazon/hop.nvim'
  --fzf
  use {'junegunn/fzf', dir = '~/.fzf', run = './install --all' }
  use {'junegunn/fzf.vim'}


end)
