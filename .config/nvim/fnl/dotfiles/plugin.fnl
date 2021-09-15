(module dotfiles.plugin
  {autoload {nvim aniseed.nvim
             a aniseed.core
             util dotfiles.util
             packer packer}})

(defn safe-require-plugin-config [name]
  (let [(ok? val-or-err) (pcall require (.. :dotfiles.plugin. name))]
    (when (not ok?)
      (print (.. "dotfiles error: " val-or-err)))))

(defn- use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (let [pkgs [...]]
    (packer.startup
      (fn [use]
        (for [i 1 (a.count pkgs) 2]
          (let [name (. pkgs i)
                opts (. pkgs (+ i 1))]
            (-?> (. opts :mod) (safe-require-plugin-config))
            (use (a.assoc opts 1 name))))))))

;; Plugins to be managed by packer.
(use
  :wbthomason/packer.nvim {}
  :Olical/aniseed {:branch :develop}
  :Olical/conjure {:branch :develop :mod :conjure}
  :Olical/nvim-local-fennel {}
  :shaunsingh/nord.nvim {:mod :nord}
  :nvim-telescope/telescope.nvim {:requires [[:nvim-lua/popup.nvim] [:nvim-lua/plenary.nvim]] :mod :telescope}
  :nvim-treesitter/nvim-treesitter {:run ":TSUpdate" :mod :treesitter}
  :ggandor/lightspeed.nvim {}
  )
