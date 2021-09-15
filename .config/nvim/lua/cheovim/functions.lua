local log = require('cheovim.logger')

-- @Summary An autocompletion function for the :Cheovim command
-- @Description Returns a list of potential matches for the currently supplied argument
function cheovim_autocomplete(_, command)
	local split_command = vim.split(command, " ")
	return vim.fn.sort(vim.tbl_filter(function(key) return key:find(split_command[#split_command]) end, { "clean-plugins", "clean-remote-configs", "deep-clean", "force-reload", "reload", "version" }), function(current, next) return current:len() > next:len() end)
end

return {

	-- @Summary Callback for executing a :Cheovim call
	-- @Description Invoked whenever the user issues a call to :Cheovim
	-- @Param  ... (vararg) - the list of arguments as returned by <f-args>
	cheovim_execute_func = function(...)
		-- Extract the first argument from that list since we should only be receiving one argument
		local arg = ({ ... })[1]

		-- If we want to print the current version then do so
		if arg == "version" then
			log.info("Cheovim Version 0.2, Written by NTBBloodbath and Vhyrro ")
		elseif arg == "reload" then -- If we would like to reload the current config on next boot then
			-- Unlink the cheovim symlink to trigger a reload on next startup
			vim.loop.fs_unlink(vim.fn.stdpath("data") .. "/site/pack/cheovim/start/cheovim")
			log.warn("Removed configuration symlink, to commence with the reload please restart Neovim.")
		elseif arg == "force-reload" then -- If we would like to fully reload our configuration then
			-- Unlink the cheovim symlink to trigger a reload on next startup
			vim.loop.fs_unlink(vim.fn.stdpath("data") .. "/site/pack/cheovim/start/cheovim")

			-- Remove the remotely installed configuration if present
			vim.cmd("silent! !rm -rf " .. vim.fn.stdpath("data") .. "/cheovim/" .. require('cheovim.loader').selected_profile)

			log.warn("Please restart your editor to commence with the full reload.")
		elseif arg == "clean-remote-configs" then -- If we would like to clean unused remote configurations then
			-- Get all the currently installed configurations and store them in a clean list
			local configs = (function()
				local result = vim.fn.glob(vim.fn.stdpath("data") .. "/cheovim/*", 0, 1)
				for i, config in ipairs(result) do
					local split = vim.split(config, "/")
					result[i] = split[#split]
				end
				return result
			end)()

			-- Get all of the configurations currently defined in the profiles.lua file
			local present_configs = (function()
				local profiles = require('cheovim.loader').profiles
				local result = {}

				for name, profile in pairs(profiles) do
					if profile[2] and profile[2].url then
						table.insert(result, name)
					end
				end

				return result
			end)()

			-- Track the amount of removed configurations
			local removed_config_count = 0

			-- For every configuration that we have installed
			for _, config in ipairs(configs) do
				-- If it is not present in the list of configs we have specified then remove it
				if not vim.tbl_contains(present_configs, config) then
					log.info("Unused configuration", config, "detected, removing...")
					vim.cmd("silent! !rm -rf " .. vim.fn.stdpath("data") .. "/cheovim/" .. config)
					removed_config_count = removed_config_count + 1
				end
			end

			-- Issue the finish message
			if removed_config_count > 0 then
				log.info("Cheovim removed", removed_config_count, "configurations.")
			else
				log.info("Cheovim removed no configurations, you're all good!")
			end
		elseif arg == "clean-plugins" then
			-- Unlink the cheovim symlink to trigger a reload on next startup
			vim.loop.fs_unlink(vim.fn.stdpath("data") .. "/site/pack/cheovim/start/cheovim")

			-- Remove all the current config's plugins
			vim.cmd("silent! !rm -rf " .. vim.fn.stdpath("data") .. "/site/pack/cheovim/" .. require('cheovim.loader').selected_profile)

			log.info("Cleaned all the current configuration's plugins!")
		elseif arg == "deep-clean" then
			vim.cmd [[
				Cheovim clean-plugins
				Cheovim force-reload
			]]
		end
	end,

	-- @Summary Invoke a post-load command specified by the user
	-- @Description Triggered whenever a configuration is switched and whenever the user has set the `config` option
	cheovim_config_callback = function()
		-- Require the cheovim loader
		local loader = require('cheovim.loader')

		-- If our profile hasn't changed since last load then don't trigger
        if not loader.profile_changed then return end

		-- Grab the configuration for our current profile
		local profile_config = loader.profiles[loader.selected_profile][2]

		-- If we have defined a config variable then
		if profile_config.config then
			-- Depending on the type of the config variable execute the code in a different way
			vim.defer_fn(vim.schedule_wrap(function()
                if type(profile_config.config) == "string" then
                    vim.cmd(profile_config.config)
                elseif type(profile_config.config) == "function" then
                    profile_config.config()
                end
            end), 100) -- We defer for 100ms here to be absolutely sure the config has loaded
		end
	end,

}
