local doom_modules = {
    "options",
    "mappings",
}

for i = 1, #doom_modules, 1 do
    pcall(require, doom_modules[i])
end
