local present, mapsheet = pcall(require, "cheatsheet")

if not present then
   return
end

local mappings = require("core.utils").mappings

local function add_to_mapsheet(section, keymap, desc)
   if section == "plugin" then
      for sec, key in pairs(mappings.plugin) do
         add_to_mapsheet(sec, key, sec)
      end
   else
      if type(keymap) == "table" then
         for sec, key in pairs(keymap) do
            if type(sec) == "number" then
               add_to_mapsheet(section, key, desc or section)
            else
               add_to_mapsheet(sec, key, desc or section)
            end
         end
      else
         mapsheet.add_cheat(section, keymap, desc or "Misc")
      end
   end
end

for section, keymap in pairs(mappings) do
   add_to_mapsheet(section, keymap)
end

require("cheatsheet").setup {

   bundled_cheatsheets = {
      enabled = { "default" },
      disabled = { "unicode", "nerd-fonts" },
   },

   bundled_plugin_cheatsheets = false,
   include_only_installed_plugins = true,
}
