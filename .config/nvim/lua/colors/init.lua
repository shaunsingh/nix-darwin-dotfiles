local M = {}

M.init = function(theme)
  vim.g.nord_borders = false
  vim.g.nord_contrast = false
  vim.g.nord_cursorline_transparent = true
  vim.g.nord_disable_background = false
  vim.cmd[[colorscheme nord]]
end

return M
