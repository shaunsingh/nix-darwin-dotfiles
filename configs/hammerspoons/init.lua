-- Load in config as modules
wm         = require('wm');
plugins    = require('plugins');
bindings   = require('bindings');

-- hide window shadows (doesn't work)
hs.window.setShadows(false)

-- Load hs on the command line
hs.ipc.cliInstall()

-- bindings
bindings.enabled = { 'grid', 'hotkeys', 'tiling' }

-- start/stop modules
local modules = {wm, plugins, bindings}

hs.fnutils.each(modules, function(module)
  if module then module.start() end
end)

-- stop modules on shutdown
hs.shutdownCallback = function()
  hs.fnutils.each(modules, function(module)
    if module then module.stop() end
  end)
end

-- watch config for changes and reload when they occur
function reloadConfig(files)  
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end


hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.notify.new({title="hammerspoon", informativeText="Config reloaded"}):send()
