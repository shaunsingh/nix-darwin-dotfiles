local module = { cache = cache }
local hyper = { "cmd", "ctrl", "shift" }

module.start = function()
	hs.loadSpoon("SpoonInstall")

	spoon.SpoonInstall.use_syncinstall = true

	Install = spoon.SpoonInstall

	Install:andUse("Caffeine", {
		start = false,
		hotkeys = {
			toggle = { hyper, "1" },
		},
	})
end

module.stop = function() end

return module
