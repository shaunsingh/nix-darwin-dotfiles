local module = { cache = cache }

module.start = function()
	local mash = { "ctrl", "cmd" }

	hs.window.animationDuration = 0

	function bindKey(key, fn)
		hs.hotkey.bind(mash, key, fn)
	end

	positions = {
		maximized = { x = 0, y = 0, w = 1, h = 1 },
		centered = { x = 0.17, y = 0.08, w = 0.66, h = 0.85 },
		center = { x = 0.1, y = 0.05, w = 0.77, h = 0.88 },

		left34 = { x = 0, y = 0, w = 0.34, h = 1 },
		left50 = hs.layout.left50,
		left66 = { x = 0, y = 0, w = 0.66, h = 1 },
		left70 = hs.layout.left70,

		right30 = hs.layout.right30,
		right34 = { x = 0.66, y = 0, w = 0.34, h = 1 },
		right50 = hs.layout.right50,
		right66 = { x = 0.34, y = 0, w = 0.66, h = 1 },

		upper50 = { x = 0, y = 0, w = 1, h = 0.5 },
		upper50Left50 = { x = 0, y = 0, w = 0.5, h = 0.5 },
		upper50Right15 = { x = 0.85, y = 0, w = 0.15, h = 0.5 },
		upper50Right30 = { x = 0.7, y = 0, w = 0.3, h = 0.5 },
		upper50Right50 = { x = 0.5, y = 0, w = 0.5, h = 0.5 },

		lower50 = { x = 0, y = 0.5, w = 1, h = 0.5 },
		lower50Left50 = { x = 0, y = 0.5, w = 0.5, h = 0.5 },
		lower50Right50 = { x = 0.5, y = 0.5, w = 0.5, h = 0.5 },

		chat = { x = 0.5, y = 0, w = 0.35, h = 0.5 },
	}

	-- Grid
	grid = {
		{ key = "h", units = { positions.left50, positions.left66, positions.left34 } },
		{ key = "j", units = { positions.lower50 } },
		{ key = "k", units = { positions.upper50 } },
		{ key = "l", units = { positions.right50, positions.right66, positions.right34 } },

		{ key = "u", units = { positions.upper50Left50 } },
		{ key = "o", units = { positions.upper50Right50 } },
		{ key = "i", units = { positions.centered, positions.center, positions.maximized } },
		{ key = ",", units = { positions.lower50Left50 } },
		{ key = ".", units = { positions.lower50Right50 } },
		{ key = "f", units = { positions.maximized } },
	}

	hs.fnutils.each(grid, function(entry)
		bindKey(entry.key, function()
			local units = entry.units
			local screen = hs.screen.mainScreen()
			local window = hs.window.focusedWindow()
			local windowGeo = window:frame()

			local index = 0
			hs.fnutils.find(units, function(unit)
				index = index + 1

				local geo = hs.geometry.new(unit):fromUnitRect(screen:frame()):floor()
				return windowGeo:equals(geo)
			end)
			if index == #units then
				index = 0
			end

			currentLayout = null
			window:moveToUnit(units[index + 1])
		end)
	end)

	--Layouts
	-- hs.hotkey.bind(mash_shift, 'j', function()
	--   hs.layout.apply({
	--     {"Firefox", nil, screen, positions.right50, nil, nil},
	--     {"iTerm2",   nil, screen, positions.left50, nil, nil}
	--   })
	-- end)
	--
	-- hs.hotkey.bind(mash_shift, 'k', function()
	--   hs.layout.apply({
	--     {"Firefox", nil, screen, positions.center, nil, nil},
	--     {"iTerm2",   nil, screen, positions.center, nil, nil}
	--   })
	-- end)

	local function testing(app, eventType, state)
		-- hs.alert(app)
		if app == "iTerm" then
			local maximized = { x = 0, y = 0, w = 1, h = 1 }
			local windowGeo = window:frame()
			local screen = hs.screen.mainScreen()
			local geo = hs.geometry.new(maximized):fromUnitRect(screen:frame()):floor()
			return windowGeo:equals(geo)
		end
	end

	local setItermSize = hs.application.watcher.new(testing)
	setItermSize:start()
end

return module
