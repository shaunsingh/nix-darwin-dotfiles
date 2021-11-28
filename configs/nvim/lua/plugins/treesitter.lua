local present, ts_config = pcall(require, "nvim-treesitter.configs")
if not present then
   return
end

local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()
parser_configs.norg = {
   install_info = {
      url = "https://github.com/nvim-neorg/tree-sitter-norg",
      files = { "src/parser.c", "src/scanner.cc" },
      branch = "main",
   },
}

ts_config.setup {
   ensure_installed = "maintained",
   indent = { enable = true },
   highlight = {
      enable = true,
      use_languagetree = true,
   },
   rainbow = {
      enable = true,
      extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
      max_file_lines = nil, -- Do not enable for files with more than n lines, int
   },
   playground = {
      enable = true,
      updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
      persist_queries = false, -- Whether the query persists across vim sessions
   },
   incremental_selection = {
      enable = true,
      keymaps = {
         init_selection = '<CR>',
         scope_incremental = '<CR>',
         node_incremental = '<TAB>',
         node_decremental = '<S-TAB>',
      },
   },
}
