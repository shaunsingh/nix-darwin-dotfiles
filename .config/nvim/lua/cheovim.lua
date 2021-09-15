-- Initialize the loader and logger
local loader, log = require('cheovim.loader'), require('cheovim.logger')

-- Grab the selected profiles from the profiles file
local selected_profile, profiles = loader.get_profiles(vim.fn.stdpath("config") .. "/profiles.lua")

-- If our supplied profile is nil then error out
if not selected_profile then
	log.error("Failed to load cheovim: profile could not be found.")
	return
end

-- Create all the plugins if necessary and start up the rest of cheovim!
loader.create_plugin_symlink(selected_profile, profiles)

 vim.cmd [[ autocmd VimEnter * ++once :lua require('cheovim.functions').cheovim_config_callback() ]]
vim.cmd [[ command -nargs=1 -complete=customlist,v:lua.cheovim_autocomplete Cheovim :lua require('cheovim.functions').cheovim_execute_func(<f-args>) ]]
