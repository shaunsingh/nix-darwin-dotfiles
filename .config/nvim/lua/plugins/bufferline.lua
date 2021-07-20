local present, bufferline = pcall(require, "bufferline")
if not present then
    return
end

local bg = "#2e3440"
local bg2 = "#3b4252"
local bg3 = "#282c34"
local fg = "#CACed6"
local accent = "#81a1c1"
local accent2 = "#BF616A" -- Not saved
local accent3 = "#EBCB8B" -- Not saved

bufferline.setup {
    options = {
    numbers = "none",
    mappings = true,
    -- NOTE: this plugin is designed with this icon in mind,
    -- and so changing this is NOT recommended, this is intended
    -- as an escape hatch for people who cannot bear it for whatever reason
    indicator_icon = '',
    buffer_close_icon = '',
    modified_icon = '●',
    close_icon = '',
    left_trunc_marker = '',
    right_trunc_marker = '',
    max_name_length = 18,
    max_prefix_length = 15, -- prefix used when a buffer is de-duplicated
    tab_size = 18,
    offsets = {
      {
          filetype = "NvimTree",
          text = "Files"
      }
    },
    show_buffer_icons = true, -- disable filetype icons for buffers
    show_buffer_close_icons = true,
    show_close_icon = false,
    show_tab_indicators = true,
    persist_buffer_sort = false, -- whether or not custom sorted buffers should persist
    -- can also be a table containing 2 custom separators
    -- [focused and unfocused]. eg: { '|', '|' }
    separator_style = "thin",
    enforce_regular_tabs = false,
    always_show_bufferline = true,
    sort_by = 'directory'
    },
    highlights = {
         fill = {
             guibg = bg
         },
        background = {
            guibg = bg
        },

        -- buffer
        buffer_selected = {
            guifg = fg,
            guibg = bg2,
            gui = "bold"
        },
        separator = {
            guifg = bg3,
            guibg = bg
        },
        separator_selected = {
            guifg = bg3,
            guibg = bg2
        },
        separator_visible = {
            guifg = bg2,
            guibg = bg2
        },
        indicator_selected = {
            guifg = accent,
            guibg = bg2
        },

        -- tabs over right
        tab = {
            guifg = fg,
            guibg = bg
        },
         tab_selected = {
            guifg = accent,
            guibg = bg2
        },
         tab_close = {
            guifg = accent,
            guibg = bg2
        },
        modified_selected = {
            guifg = accent2,
            guibg = bg2
        },
        modified = {
            guifg = accent3,
            guibg = bg
        },
        modified_visible = {
            guifg = accent,
            guibg = bg
        }
    }
}
