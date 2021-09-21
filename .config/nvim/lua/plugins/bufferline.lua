local present, bufferline = pcall(require, "bufferline")
if not present then
   return
end

-- function executed for top right close button in bufferline
vim.cmd "function! Doom_bufferline_quitvim(a,b,c,d) \n qa \n endfunction"

local colors = {
  bg = "NONE",
  black = "#3b4252",
  black2 = "#2E3440",
  white = "#ECEFF4",
  fg = "#E5E9F0",
  yellow = "#EBCB8B",
  cyan = "#A3BE8C",
  darkblue = "#81A1C1",
  green = "#8FBCBB",
  orange = "#D08770",
  purple = "#B48EAD",
  magenta = "#BF616A",
  gray = "#616E88",
  blue = "#5E81AC",
  red = "#BF616A"
}

bufferline.setup {
   options = {
      offsets = { { filetype = "NvimTree", text = "", padding = 1 } },
      buffer_close_icon = "",
      modified_icon = "",
      close_icon = "",
      show_close_icon = true,
      left_trunc_marker = "",
      right_trunc_marker = "",
      max_name_length = 14,
      max_prefix_length = 13,
      tab_size = 20,
      show_tab_indicators = true,
      enforce_regular_tabs = false,
      view = "multiwindow",
      show_buffer_close_icons = true,
      separator_style = "thin",
      always_show_bufferline = false,
      diagnostics = false, -- "or nvim_lsp"
      custom_filter = function(buf_number)
         -- Func to filter out our managed/persistent split terms
         local present_type, type = pcall(function()
            return vim.api.nvim_buf_get_var(buf_number, "term_type")
         end)

         if present_type then
            if type == "vert" then
               return false
            elseif type == "hori" then
               return false
            else
               return true
            end
         else
            return true
         end
      end,
   },

   highlights = {
      background = {
         guifg = colors.fg,
         guibg = colors.black2,
      },

      -- buffers
      buffer_selected = {
         guifg = colors.white,
         guibg = colors.black,
         gui = "bold",
      },
      buffer_visible = {
         guifg = colors.gray,
         guibg = colors.black2,
      },

      -- for diagnostics = "nvim_lsp"
      error = {
         guifg = colors.gray,
         guibg = colors.black2,
      },
      error_diagnostic = {
         guifg = colors.gray,
         guibg = colors.black2,
      },

      -- close buttons
      close_button = {
         guifg = colors.gray,
         guibg = colors.black2,
      },
      close_button_visible = {
         guifg = colors.gray,
         guibg = colors.black2,
      },
      close_button_selected = {
         guifg = colors.red,
         guibg = colors.black,
      },
      fill = {
         guifg = colors.fg,
         guibg = colors.black2,
      },
      indicator_selected = {
         guifg = colors.black,
         guibg = colors.black,
      },

      -- modified
      modified = {
         guifg = colors.red,
         guibg = colors.black2,
      },
      modified_visible = {
         guifg = colors.red,
         guibg = colors.black2,
      },
      modified_selected = {
         guifg = colors.gree,
         guibg = colors.black,
      },

      -- separators
      separator = {
         guifg = colors.black2,
         guibg = colors.black2,
      },
      separator_visible = {
         guifg = colors.black2,
         guibg = colors.black2,
      },
      separator_selected = {
         guifg = colors.black2,
         guibg = colors.black2,
      },
      -- tabs
      tab = {
         guifg = colors.fg,
         guibg = colors.gray,
      },
      tab_selected = {
         guifg = colors.black2,
         guibg = colors.fg,
      },
      tab_close = {
         guifg = colors.red,
         guibg = colors.black,
      },
   },
}
