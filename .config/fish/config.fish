if not set -q TMUX
    set -g TMUX tmux new-session -d -s base
    eval $TMUX
    tmux attach-session -d -t base
end

set fish_greeting

alias neovide "/Users/shauryasingh/IdeaProjects/neovim/neovide/target/release/neovide --multiGrid --frameless"

set -x EDITOR "vim"
set -x PATH .emacs.d/bin $PATH
set -x PATH /Library/TeX/texbin/ $PATH
set PATH $HOME/.cargo/bin $PATH

starship init fish | source
