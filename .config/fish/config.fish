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

set -x EDITOR "vim"
set -x PATH .emacs.d/bin $PATH
set -x PATH /Library/TeX/texbin/ $PATH
set PATH $HOME/.cargo/bin $PATH

# Add fenv to path
set fish_function_path $fish_function_path ~/IdeaProjects/plugin-foreign-env/functions

# Source Nix setup script
fenv source ~/.nix-profile/etc/profile.d/nix.sh

starship init fish | source
fish_add_path /opt/homebrew/opt/openssl@1.1/bin
