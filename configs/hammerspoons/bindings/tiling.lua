local module = {}
local hhtwm  = wm.cache.hhtwm

local keys   = {"ctrl", "cmd"}
hs.window.animationDuration = 0

module.start = function()
    local bind = function(key, fn)
        hs.hotkey.bind(keys, key, fn, nil, fn)
    end

    -- toggle [f]loat
    bind('w', function()
        local win = hs.window.frontmostWindow()

        if not win then return end

        hhtwm.toggleFloat(win)

        if hhtwm.isFloating(win) then
            hs.grid.center(win)
        end

        highlightWindow()
    end)



    -- bind('b', function() hhtwm.setLayout('cards') end)
    bind(']', function() 
        wm.cycleLayout()
    end)


    -- [r]eset
    bind('r', hhtwm.reset)

    -- re[t]ile
    bind('t', hhtwm.tile)

    -- [e]qualize
    bind('e', hhtwm.equalizeLayout)

    -- toggle [z]oom window
    bind('z', function()
        local win = hs.window.frontmostWindow()

        if not hhtwm.isFloating(win) then
            hhtwm.toggleFloat(win)
            hs.grid.maximizeWindow(win)
        else
            hhtwm.toggleFloat(win)
        end

        highlightWindow()
    end)

    -- throw window to space (and move)
    for n = 0, 9 do
        local idx = tostring(n)

        -- important: use this with onKeyReleased, not onKeyPressed
        hs.hotkey.bind(keys, idx, nil, function()
            local win = hs.window.focusedWindow()

            -- if there's no focused window, just move to that space
            if not win then
                hs.eventtap.keyStroke({ 'ctrl' }, idx)
                return
            end

            local isFloating = hhtwm.isFloating(win)
            local success    = hhtwm.throwToSpace(win, n)

            -- if window switched space, then follow it (ctrl + 0..9) and focus
            if success then
                hs.eventtap.keyStroke({ 'ctrl' }, idx)

                -- retile and re-highlight window after we switch space
                hs.timer.doAfter(0.05, function()
                    if not isFloating then hhtwm.tile() end
                    highlightWindow(win)
                end)
            end
        end)
    end
end

module.stop = function()
end

return module
