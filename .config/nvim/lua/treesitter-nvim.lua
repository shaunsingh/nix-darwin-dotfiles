local M = {}

M.config = function()
    local ts_config = require("nvim-treesitter.configs")

    ts_config.setup {
        ensure_installed = {
            "java",
            "kotlin",
            "comment",
            "bash",
            "lua",
            "fish",
            "python",
            "json",
            "latex",
            "rust",
            "toml",
        },
        highlight = {
            enable = true,
            use_languagetree = true
        }
    }
end

return M
