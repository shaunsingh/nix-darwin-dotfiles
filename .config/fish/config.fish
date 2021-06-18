function fish_greeting
    set -l toon (random choice {default,bud-frogs,dragon,dragon-and-cow,elephant,moose,stegosaurus,tux,vader})
    if which fortune > /dev/null ^ /dev/null
       fortune -s | cowsay -f $toon
   else
       echo Something fishy going on around here ...
   end
end

set fish_greeting

alias neovide "/Users/shauryasingh/IdeaProjects/neovim/neovide/target/release/neovide --multiGrid --frameless"

set -x EDITOR "nvim"
set -x PATH .emacs.d/bin $PATH
set PATH $HOME/.cargo/bin $PATH
fish_add_path /opt/homebrew/sbin

starship init fish | source
