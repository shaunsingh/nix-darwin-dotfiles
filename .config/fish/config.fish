if status is-interactive
and not set -q TMUX
    exec tmux
end

set fish_greeting

alias neovide "~/IdeaProjects/neovim/neovide/target/release/neovide --multiGrid --frameless"

alias .. "cd .."
alias ... "cd ../.."
alias .... "cd ../../.."
alias ..... "cd ../../../.."
alias ll 'exa -lF --color-scale --no-user --no-time --no-permissions --group-directories-first -a'
alias ls 'exa -F --group-directories-first -a'
alias tree 'tree -C'
alias cat 'bat --paging=never -p'

alias git-rebase 'git rebase -i HEAD~2'

alias update 'brew update; brew upgrade; brew cleanup'
alias cleanup "find . -type f -name '*.DS_Store' -ls -delete"

set -x EDITOR "vim"
set -x PATH ~/.emacs.d/bin $PATH
set -x PATH /Library/TeX/texbin/ $PATH
set -x PATH ~/.config/scripts $PATH
set PATH $HOME/.cargo/bin $PATH

starship init fish | source
