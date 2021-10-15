local present, ts_config = pcall(require, "nvim-treesitter.configs")
if not present then
   return
end

local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.org = {
  install_info = {
    url = 'https://github.com/milisims/tree-sitter-org',
    revision = 'main',
    files = {'src/parser.c', 'src/scanner.cc'},
  },
  filetype = 'org',
}

ts_config.setup {
   ensure_installed = "lua",
   indent = { enable = true },
   highlight = {
      enable = true,
      use_languagetree = true,
   },
   autopairs = { enable = true },
   rainbow = {
      enable = true,
      extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
      max_file_lines = nil, -- Do not enable for files with more than n lines, int
   },
   playground = {
      enable = true,
      updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
      persist_queries = false, -- Whether the query persists across vim sessions
   }
}
