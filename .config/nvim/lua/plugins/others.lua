local M = {}

M.colorizer = function()
    local present, colorizer = pcall(require, "colorizer")
    if present then
        colorizer.setup()
        vim.cmd("ColorizerReloadAllBuffers")
    end
end

M.comment = function()
    local present, nvim_comment = pcall(require, "nvim_comment")
    if present then
        nvim_comment.setup()
    end
end

M.lspkind = function()
    local present, lspkind = pcall(require, "lspkind")
    if present then
        lspkind.init()
    end
end

M.neoscroll = function()
    pcall(
        function()
            require("neoscroll").setup()
        end
    )
end

M.blankline = function()
   require("indent_blankline").setup {
      indentLine_enabled = 1,
      char = "▏",
      indent_blankline_filetype_exclude = { "help", "terminal", "dashboard", "packer" },
      indent_blankline_buftype_exclude = { "terminal" },
      indent_blankline_show_trailing_blankline_indent = false,
      indent_blankline_show_first_indent_level = false,
   }
end

M.escape = function()
    vim.g.better_escape_interval = 300
    vim.g.better_escape_shortcut = {"jk"}
end

return M
