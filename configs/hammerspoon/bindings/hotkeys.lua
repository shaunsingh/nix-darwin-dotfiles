local module = { cache = cache }
local hyper = { "cmd", "ctrl", "shift" }

module.start = function()
	---------------------------------------------------------
	-- App Hotkeys
	---------------------------------------------------------

	hs.hotkey.bind(hyper, "J", function()
		hs.application.launchOrFocus("Alacritty")
	end)
	hs.hotkey.bind(hyper, "K", function()
		hs.application.launchOrFocus("Firefox")
	end)
	hs.hotkey.bind(hyper, "T", function()
		hs.application.launchOrFocus("Transmit")
	end)
	hs.hotkey.bind(hyper, "M", function()
		hs.application.launchOrFocus("Mail")
	end)
	hs.hotkey.bind(hyper, "G", function()
		hs.application.launchOrFocus("Tower")
	end)

	---------------------------------------------------------
	-- ON-THE-FLY KEYBIND
	---------------------------------------------------------

	-- Temporarily bind an application to be toggled by the V key
	-- useful for once-in-a-while applications like Preview
	local boundApplication = nil

	hs.hotkey.bind(hyper, "C", function()
		local appName = hs.window.focusedWindow():application():title()

		if boundApplication then
			boundApplication:disable()
		end

		boundApplication = hs.hotkey.bind(hyper, "V", function()
			hs.application.launchOrFocus(appName)
		end)

		-- https://github.com/Hammerspoon/hammerspoon/issues/184#issuecomment-102835860
		boundApplication:disable()
		boundApplication:enable()

		hs.alert(string.format('Binding: "%s" => hyper + V', appName))
	end)

	---------------------------------------------------------
	-- Simulate arrow keys with Hyper
	---------------------------------------------------------
	-- https://stackoverflow.com/questions/41025343/when-remapping-hyper-key-caps-lock-to-f18-with-hammerspoon-is-it-possible-to-us

	arrowKey = function(arrow, modifiers)
		local event = require("hs.eventtap").event
		event.newKeyEvent(modifiers, string.lower(arrow), true):post()
		event.newKeyEvent(modifiers, string.lower(arrow), false):post()
	end

	hs.hotkey.bind(hyper, "N", function()
		arrowKey("DOWN")
	end)
	hs.hotkey.bind(hyper, "P", function()
		arrowKey("UP")
	end)

	---------------------------------------------------------
	-- Misc
	---------------------------------------------------------

	hs.hotkey.bind(hyper, "X", function()
		hs.openConsole()
		hs.focus()
	end)

	-- Reload config
	hs.hotkey.bind(hyper, "R", function()
		hs.reload()
	end)
end

return module
