--Load main config
local doom_modules = {
    "options",
    "mappings",
    "utils"
}

for i = 1, #doom_modules, 1 do
    pcall(require, doom_modules[i])
end
