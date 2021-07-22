require "options"

local doom_modules = {
    "pluginList",
    "plugins.bufferline",
    "mappings",
    "utils"
}

local async
async =
    vim.loop.new_async(
    vim.schedule_wrap(
        function()
            for i = 1, #doom_modules, 1 do
                local ok, res = xpcall(require, debug.traceback, doom_modules[i])
                if not (ok) then
                    print("Error loading module : " .. doom_modules[i])
                    print(res) -- print stack traceback of the error
                end
            end
            async:close()
        end
    )
)
async:send()
