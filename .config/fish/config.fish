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
set -x PATH /opt/homebrew/Cellar/python@3.9/3.9.5/libexec/bin $PATH
set -x PATH /Users/shauryasingh/opt/anaconda3/bin $PATH
set -x TERM xterm-256color
set -x LC_ALL en_US.UTF-8
set -x LANG en_US.UTF-8
set -x LANGUAGE en_US.UTF-8
set PATH $HOME/.cargo/bin $PATH

starship init fish | source
fish_add_path /opt/homebrew/sbin

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /Users/shauryasingh/opt/anaconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<
