local alpha = require "alpha"
local dashboard = require "alpha.themes.dashboard"
vim.cmd [[autocmd FileType alpha set laststatus=0 | autocmd BufUnload <buffer> set laststatus=2]]
dashboard.section.header.val = {
   [[=================     ===============     ===============   ========  ========]],
   [[\\ . . . . . . .\\   //. . . . . . .\\   //. . . . . . .\\  \\. . .\\// . . //]],
   [[||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\/ . . .||]],
   [[|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||]],
   [[||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||]],
   [[|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\ . . . . ||]],
   [[||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\_ . .|. .||]],
   [[|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\ `-_/| . ||]],
   [[||_-' ||  .|/    || ||    \|.  || `-_|| ||_-' ||  .|/    || ||   | \  / |-_.||]],
   [[||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \  / |  `||]],
   [[||    `'         || ||         `'    || ||    `'         || ||   | \  / |   ||]],
   [[||            .===' `===.         .==='.`===.         .===' /==. |  \/  |   ||]],
   [[||         .=='   \_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \/  |   ||]],
   [[||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \/  |   ||]],
   [[||   .=='    _-'          '-__\._-'         '-_./__-'         `' |. /|  |   ||]],
   [[||.=='    _-'                                                     `' |  /==.||]],
   [[=='    _-'                        N E O V I M                         \/   `==]],
   [[\   _-'                                                                `-_   /]],
   [[ `''                                                                      ``' ]],
}

dashboard.section.buttons.val = {
   dashboard.button("SPC f .", "  Find File", ":Telescope find_files<CR>"),
   dashboard.button("SPC f r", "  Recents  ", ":Telescope oldfiles<CR>"),
   dashboard.button("SPC w w", "  Find Word", ":Telescope live_grep<CR>"),
   dashboard.button("SPC b m", "  Bookmarks", ":Telescope marks<CR>"),
   dashboard.button(":qa!   ", "  Quit     ", ":qa<CR>"),
}
alpha.setup(dashboard.opts)
