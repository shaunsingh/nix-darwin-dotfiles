require "options"

local doom_modules = {
    "pluginList",
    "mappings",
    "utils"
}

local async
async =
    vim.loop.new_async(
    vim.schedule_wrap(
        function()
            for i = 1, #doom_modules, 1 do
                 pcall(require, doom_modules[i])
            end
            async:close()
        end
    )
)
async:send()
