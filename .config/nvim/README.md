```
      ___           ___           ___           ___                                    ___     
     /  /\         /__/\         /  /\         /  /\          ___        ___          /__/\    
    /  /:/         \  \:\       /  /:/_       /  /::\        /__/\      /  /\        |  |::\   
   /  /:/           \__\:\     /  /:/ /\     /  /:/\:\       \  \:\    /  /:/        |  |:|:\  
  /  /:/  ___   ___ /  /::\   /  /:/ /:/_   /  /:/  \:\       \  \:\  /__/::\      __|__|:|\:\ 
 /__/:/  /  /\ /__/\  /:/\:\ /__/:/ /:/ /\ /__/:/ \__\:\  ___  \__\:\ \__\/\:\__  /__/::::| \:\
 \  \:\ /  /:/ \  \:\/:/__\/ \  \:\/:/ /:/ \  \:\ /  /:/ /__/\ |  |:|    \  \:\/\ \  \:\~~\__\/
  \  \:\  /:/   \  \::/       \  \::/ /:/   \  \:\  /:/  \  \:\|  |:|     \__\::/  \  \:\      
   \  \:\/:/     \  \:\        \  \:\/:/     \  \:\/:/    \  \:\__|:|     /__/:/    \  \:\     
    \  \::/       \  \:\        \  \::/       \  \::/      \__\::::/      \__\/      \  \:\    
     \__\/         \__\/         \__\/         \__\/           ~~~~                   \__\/    
```

<div align="center">

![License](https://img.shields.io/github/license/NTBBloodbath/cheovim?style=flat-square)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](http://makeapullrequest.com)
![Neovim version](https://img.shields.io/badge/Neovim-0.5-57A143?style=flat-square&logo=neovim)

![Cheovim Showcase GIF](res/cheovim.gif)

Neovim configuration switcher written in Lua. Inspired by chemacs.

[Introduction](#star2-introduction) • [Installation](#clock4-installation) • [Configuration](#wrench-configuration)

</div>

---

# :star2: Introduction
Cheovim is a plugin designed to make your life easier by being able to manage several Neovim
configurations simultaneously and allow you to fully seamlessly transition between them.

By default your configuration is stored under a static `~/.config/nvim` directory. This means if you want to try out
another chad's configuration you must first move away your configuration into a backup folder, install the other person's config,
put it in the correct spot, clear your `site/pack` directory so that the plugin manager can work without conflicts,
install the plugin manager, install the plugins and *then* start using the config. That's absolutely atrocious!
Afterwards switching back is the exact same awful process. 

But what if we told you all of that could be fully automated?

# :clock4: Installation
> :exclamation: Cheovim requires at least Neovim 0.5+ to operate, and may not work on Windows machines! 

Cheovim only works with configurations that use the new `init.lua` configuration style. If we get enough requests we will
implement `init.vim` support too, but for now keep up with the times!

To install cheovim, make sure to first move your configuration (located under `~/.config/nvim/`) out of the way (into e.g. `~/.config/nvim.bak/`).
Afterwards be sure to clear everything from `~/.local/share/nvim/site/pack/`, this is the directory where all of your current plugins are installed.
Cheovim uses symlinks as part of its magic and needs this directory to be clean. Optionally, to make your life easier, remove any files inside your config
that may autorun on startup and attempt to load plugins (like `plugin/packer_compiled.vim`) - this isn't necessary but you'll see that it makes the initial
install a bunch easier in some cases.

Then we can start the installation!
```sh
git clone https://github.com/NTBBloodbath/cheovim ~/.config/nvim
```
Will clone cheovim into the config directory. You're almost ready to go! Now we just have to tell cheovim which configs to use:
Navigate to `~/.config/nvim/` and open the `profiles.lua` file - this is where all the configuration resides.

This is the default file:
```lua
--[[
     /  /\         /__/\         /  /\         /  /\          ___        ___          /__/\    
    /  /:/         \  \:\       /  /:/_       /  /::\        /__/\      /  /\        |  |::\   
   /  /:/           \__\:\     /  /:/ /\     /  /:/\:\       \  \:\    /  /:/        |  |:|:\  
  /  /:/  ___   ___ /  /::\   /  /:/ /:/_   /  /:/  \:\       \  \:\  /__/::\      __|__|:|\:\ 
 /__/:/  /  /\ /__/\  /:/\:\ /__/:/ /:/ /\ /__/:/ \__\:\  ___  \__\:\ \__\/\:\__  /__/::::| \:\
 \  \:\ /  /:/ \  \:\/:/__\/ \  \:\/:/ /:/ \  \:\ /  /:/ /__/\ |  |:|    \  \:\/\ \  \:\~~\__\/
  \  \:\  /:/   \  \::/       \  \::/ /:/   \  \:\  /:/  \  \:\|  |:|     \__\::/  \  \:\      
   \  \:\/:/     \  \:\        \  \:\/:/     \  \:\/:/    \  \:\__|:|     /__/:/    \  \:\     
    \  \::/       \  \:\        \  \::/       \  \::/      \__\::::/      \__\/      \  \:\    
     \__\/         \__\/         \__\/         \__\/           ~~~~                   \__\/    

	A config switcher written in Lua by NTBBloodbath and Vhyrro.
--]]

-- Defines the profiles you want to use
local profiles = {
	--[[
	Here's an example:

		<name_of_config> = { <path_to_config>, {
				plugins = "packer", -- Where to install plugins under site/pack
				preconfigure = "packer:opt" -- Whether or not to preconfigure a plugin manager for you
			} 
		}

	More in-depth information can be found in cheovim's README on GitHub.
	--]]
	my_config = { "<path>", {
			plugins = "packer",
			preconfigure = "packer",
		}
	},
}

-- return <name_of_config>, <list_of_profiles>
return "my_config", profiles
```

You can tweak this file as you see fit, although we recommend reading the next section to actually know what you're doing.

# :wrench: Configuration
Cheovim has a fair amount of configuration that may not become apparent right off the bat. Here's the things that you can change:
```lua
local profiles = {
	my_config = { "/my/path", {
			url = false,
			setup = function()
				os.remove("some_important_file")
			end,
			config = "PackerSync",
			plugins = "<plugin_location>",
			preconfigure = "{packer|paq-nvim}:{start|opt}:<branch> | 
				doom-nvim | lunarvim | nv-ide | vapournvim | lvim",
		}
	}
}

return "my_config", profiles
```

### Options
- `plugins = "<plugin_location>"` - where under `~/.local/share/nvim/site/pack/` to install plugins. Defaults to `"packer"`, meaning plugins
will be installed under `~/.local/share/nvim/site/pack/packer`.
By default different plugin managers will install themselves into different directories. Packer installs itself into `packer`,
paq-nvim installs itself into `paq-nvim`, etc. There may be times where different configs install their plugin managers into
different directories altogether, so you can change this value accordingly.
- `preconfigure = "<config>"` - preconfigure a plugin manager before switching configs. Useful for seamless config transitions.
Options are divided with `:`, but you needn't supply all of them. For example, a value of `packer:opt:fix/premature-display-opening`
means "preconfigure packer, install it as an opt plugin, and use the `fix/premature-display-opening` branch". Not supplying parts of these parameters
will use the default values instead, so `packer::fix/premature-display-opening` will be the equivalent of `packer:start:fix/premature-display-opening`,
`packer:opt` will be the equivalent of `packer:opt:master` and finally `packer` is the equivalent of `packer:start:master`.
The same principles apply to `paq-nvim`. It is also possible to supply a name of a known configuration and cheovim will set up the plugin manager the way
that configuration requires it, so e.g. setting `preconfigure` to `"doom-nvim"` will automatically set up packer the way [doom-nvim](https://github.com/NTBBloodbath/doom-nvim)
likes it. This value can be set to `nil` too to perform no preconfiguration.
- `url` - tells cheovim whether the supplied URI is actually a link to a repository. If it is, cheovim
will first grab it from the internet, then it will boot into the configuration. Note that this slightly decreases startup
time for that configuration in general.
- `setup` - can be either a function or a string, if it is a string it will get wrapped in a vim.cmd, so be warned.
Gets invoked before the new configuration gets loaded.
- `config` - can also be either a function or a string. Gets invoked after the configuration has been fully loaded. Designed
to also work with asynchronous configurations.

- `return "my_config", profiles` - selects a configuration from a list of profiles. Make sure the first returned value has the same name
as the key inside the `profiles` table (i.e. if I define a config called `my_config = {}` make sure to return `"my_config"`). Changing this will automatically
switch configs the next time you launch Neovim.

Upon launching a new config that hasn't been preconfigured yet you will be greeted with a random message, can you find em' all?

### The Cheovim Command
Ohoho, it doesn't stop there. Cheovim also supplies a `:Cheovim` command that you can use to clean up your configs, remove plugins,
force reload the configuration on next boot, etc. These are the subcommands available:

- `:Cheovim reload` - reloads the current configuration on next boot
- `:Cheovim version` - prints the current version of cheovim
- `:Cheovim deep-clean` - cleans all the configuration's plugins, deletes the current remote config
  if present and force reloads on next boot
- `:Cheovim force-reload` - reloads the current configuration on next boot and also
  deletes the current remote config if present (remote configs are those that are pulled down via setting the url option to true)
  for a completely clean start
- `:Cheovim clean-plugins` - goes ahead and removes all the plugins from your current configuration
- `:Cheovim clean-remote-configs` - removes all of the remote configurations that aren't defined in
  your `profiles.lua` file, therefore removing unused leftover junk configs


# TODO
- [x] Add support for supplying URLs rather than regular paths to be able to check out different configurations without even needing to clone them yourself
- [x] Add the ability to execute a lua function or some vimscript code after switching configs via the config variable (inspired by packer)
- [ ] Documentation for the above features
- [ ] Hot reloading of configs whenever a change to the profiles.lua file is detected

###### Made with love by [NTBBloodbath](https://github.com/NTBBloodbath) and [Vhyrro](https://github.com/vhyrro/) :heart:
