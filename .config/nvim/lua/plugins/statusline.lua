local present1, gl = pcall(require, "galaxyline")
local present2, condition = pcall(require, "galaxyline.condition")
if not (present1 or present2) then
    return
end

local gls = gl.section

gl.short_line_list = {" "}

--[[
local colors = {
  bg = "NONE",
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
]]--

local colors = {
  bg = "NONE",
  white = "#bbc2cf",
  fg = "#bbc2cf",
  yellow = "#FCCE7B",
  cyan = "#7bc275",
  darkblue = "#51afef",
  green = "#7bc275",
  orange = "#e69055",
  purple = "#C57BDB",
  magenta = "#C57BDB",
  gray = "#9ca0a4",
  blue = "#5cEfFF",
  red = "#ff665c"
}

gls.left[1] = {
    RainbowRed = {
    provider = function() return '▊ ' end,
    highlight = {colors.blue,colors.bg}
  },
}
gls.left[2] = {
    ViMode = {
        provider = function()
            -- auto change color according the vim mode
            local mode_color = {
                n = colors.cyan, i = colors.blue, v = colors.yellow,
                [''] = colors.orange, V = colors.yellow,
                c = colors.magenta, no = colors.green, s = colors.green,
                S = colors.orange, [''] = colors.orange,
                ic = colors.yellow, R = colors.magenta, Rv = colors.magenta,
                cv = colors.red, ce = colors.red, r = colors.cyan,
                rm = colors.cyan, ['r?'] = colors.cyan,
                ['!'] = colors.red, t = colors.red
            }
            vim.api.nvim_command('hi GalaxyViMode guifg='..mode_color[vim.fn.mode()])
            return '  '
        end,
        highlight = {colors.red,colors.bg,'bold'},
    },
}
gls.left[3] = {
  Space = {
    provider = function () return ' ' end
  }
}
gls.left[4] = {
    FileSize = {
        provider = 'FileSize',
        separator = '',
        condition = condition.buffer_not_empty,
        highlight = {colors.fg,colors.bg}
    },
}
gls.left[5] = {
  Space = {
    provider = function () return ' ' end
  }
}
gls.left[6] ={
    FileIcon = {
        provider = 'FileIcon',
        condition = condition.buffer_not_empty,
        highlight = {require('galaxyline.provider_fileinfo').get_file_icon_color,colors.bg},
    },
}
gls.left[7] = {
    FileName = {
        provider = {'FileName'},
        separator = '',
        condition = condition.buffer_not_empty,
        highlight = {colors.green, colors.bg,'bold'}
    }

}
gls.left[8] = {
  Space = {
    provider = function () return ' ' end
  }
}
gls.left[9] = {
    LineInfo = {
        provider = 'LineColumn',
        separator = '',
        separator_highlight = {'NONE',colors.bg},
        highlight = {colors.fg,colors.bg},
    },
}
gls.left[10] = {
    PerCent = {
        provider = 'LinePercent',
        separator = '',
        separator_highlight = {'NONE',colors.bg},
        highlight = {colors.grey,colors.bg,'bold'},
    }
}
gls.left[11] = {
    DiagnosticError = {
        provider = 'DiagnosticError',
        icon = '  ',
        highlight = {colors.red,colors.bg}
    }
}
gls.left[12] = {
    DiagnosticWarn = {
        provider = 'DiagnosticWarn',
        icon = '  ',
        highlight = {colors.yellow,colors.bg},
    }
}
gls.left[13] = {
    DiagnosticHint = {
        provider = 'DiagnosticHint',
        icon = '  ',
        highlight = {colors.cyan,colors.bg},
    }
}
gls.left[14] = {
    DiagnosticInfo = {
        provider = 'DiagnosticInfo',
        icon = '  ',
        highlight = {colors.blue,colors.bg},
    }
}
gls.right[1] = {
    FileEncode = {
        provider = 'FileEncode',
        condition = condition.hide_in_width,
        separator = '  ',
        separator_highlight = {'NONE',colors.bg},
        highlight = {colors.green,colors.bg,'bold'}
    }
}

gls.right[2] = {
    ShowLspClient = {
        provider = 'GetLspClient',
        condition = function ()
          local tbl = {['dashboard'] = true,['']=true}
          if tbl[vim.bo.filetype] then
            return false
          end
          return true
        end,
        icon = '   ',
        highlight = {colors.yellow,colors.bg,'bold'}
    }
}
gls.right[3] = {
    FileFormat = {
        provider = 'FileFormat',
        condition = condition.hide_in_width,
        separator = '  ',
        separator_highlight = {'NONE',colors.bg},
        highlight = {colors.green,colors.bg,'bold'}
    }
}
gls.right[4] = {
    GitIcon = {
        provider = function() return '  ' end,
        condition = condition.check_git_workspace,
        separator = ' ',
        separator_highlight = {'NONE',colors.bg},
        highlight = {colors.magenta,colors.bg,'bold'},
    }
}
gls.right[5] = {
    GitBranch = {
        provider = 'GitBranch',
        condition = condition.check_git_workspace,
        highlight = {colors.magenta,colors.bg,'bold'},
    }
}
gls.right[6] = {
    DiffAdd = {
        provider = 'DiffAdd',
        condition = condition.hide_in_width,
        icon = '    ',
        highlight = {colors.green,colors.bg},
    }
}
gls.right[7] = {
    DiffModified = {
        provider = 'DiffModified',
        condition = condition.hide_in_width,
        icon = ' 柳',
        highlight = {colors.orange,colors.bg},
    }
}
gls.right[8] = {
    DiffRemove = {
        provider = 'DiffRemove',
        condition = condition.hide_in_width,
        icon = '  ',
        highlight = {colors.red,colors.bg},
    }
}
gls.right[9] = {
  Space = {
    provider = function () return ' ' end
  }
}
gls.right[10] = {
    RainbowRed = {
    provider = function() return ' ▊' end,
    highlight = {colors.blue,colors.bg}
  },
}
gls.short_line_left[1] = {
    BufferType = {
        provider = 'FileTypeName',
        separator = ' ',
        separator_highlight = {'NONE',colors.bg},
        highlight = {colors.blue,colors.bg,'bold'}
    }
}
gls.short_line_left[2] = {
    SFileName = {
        provider =  'SFileName',
        condition = condition.buffer_not_empty,
        highlight = {colors.fg,colors.bg,'bold'}
    }
}

gls.short_line_right[1] = {
    BufferIcon = {
        provider= 'BufferIcon',
        highlight = {colors.fg,colors.bg}
    }
}
