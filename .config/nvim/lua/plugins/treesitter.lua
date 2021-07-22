local present, ts_config = pcall(require, "nvim-treesitter.configs")
if not present then
    return
end

ts_config.setup {
        ensure_installed = {
            "java",
            "kotlin",
            "comment",
            "bash",
            "lua",
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
