vim.cmd("packadd packer.nvim")

local present, packer = pcall(require, "packer")

if not present then
    local packer_path = vim.fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"

    print("Cloning packer..")
    -- remove the dir before cloning
    vim.fn.delete(packer_path, "rf")
    vim.fn.system(
        {
            "git",
            "clone",
            "https://github.com/wbthomason/packer.nvim",
            "--depth",
            "20",
            packer_path
        }
    )

    vim.cmd("packadd packer.nvim")
    present, packer = pcall(require, "packer")

    if present then
        print("Packer cloned successfully.")
    else
        error("Couldn't clone packer !\nPacker path: " .. packer_path)
    end
end

return packer.init {
  profile = {
    enable = true,
    threshold = 1 -- the amount in ms that a plugins load time must be over for it to be included in the profile
  },
  display = {
     open_fn = function()
        return require("packer.util").float { border = "single" }
     end,
     prompt_border = "single",
  },
  git = {
     clone_timeout = 600, -- Timeout, in seconds, for git clones
  },
  auto_clean = true,
}
