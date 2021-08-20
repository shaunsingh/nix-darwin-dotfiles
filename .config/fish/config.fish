# launch tmux if not active
if status is-interactive
and not set -q TMUX
    exec tmux
end

# remove greating
set fish_greeting

# colors
set -U fish_color_autosuggestion      brblack
set -U fish_color_cancel              -r
set -U fish_color_command             green
set -U fish_color_comment             magenta
set -U fish_color_cwd                 green
set -U fish_color_cwd_root            red
set -U fish_color_end                 magenta
set -U fish_color_error               red
set -U fish_color_escape              cyan
set -U fish_color_history_current     --bold
set -U fish_color_host                normal
set -U fish_color_normal              normal
set -U fish_color_operator            cyan
set -U fish_color_param               blue
set -U fish_color_quote               yellow
set -U fish_color_redirection         yellow
set -U fish_color_search_match        'yellow' '--background=brightblack'
set -U fish_color_selection           'white' '--bold' '--background=brightblack'
set -U fish_color_status              red
set -U fish_color_user                green
set -U fish_color_valid_path          --underline
set -U fish_pager_color_completion    normal
set -U fish_pager_color_description   yellow
set -U fish_pager_color_prefix        'white' '--bold' '--underline'
set -U fish_pager_color_progress      'white' '--background=cyan'

# prompt
set fish_prompt_pwd_dir_length 1
set __fish_git_prompt_show_informative_status 1

set fish_color_command green
set fish_color_param $fish_color_normal

set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showupstream 'yes'

set __fish_git_prompt_color_branch brown
set __fish_git_prompt_color_dirtystate FCBC47
set __fish_git_prompt_color_stagedstate yellow
set __fish_git_prompt_color_upstream cyan
set __fish_git_prompt_color_cleanstate green
set __fish_git_prompt_color_invalidstate red

set __fish_git_prompt_char_dirtystate '*'
set __fish_git_prompt_char_stateseparator ' '
set __fish_git_prompt_char_untrackedfiles ' …'
set __fish_git_prompt_char_cleanstate '✓'
set __fish_git_prompt_char_stagedstate '⇢ '
set __fish_git_prompt_char_conflictedstate "✕"

set __fish_git_prompt_char_upstream_prefix ''
set __fish_git_prompt_char_upstream_equal ''
set __fish_git_prompt_char_upstream_ahead '⇡'
set __fish_git_prompt_char_upstream_behind '⇣'
set __fish_git_prompt_char_upstream_diverged '⇡⇣'

function _print_in_color
  set -l string $argv[1]
  set -l color  $argv[2]

  set_color $color
  printf $string
  set_color normal
end

function _prompt_color_for_status
  if test $argv[1] -eq 0
    echo magenta
  else
    echo red
  end
end

function fish_prompt
  set -l last_status $status

  _print_in_color ""(prompt_pwd) blue

  __fish_git_prompt " (%s)"

  _print_in_color " λ " (_prompt_color_for_status $last_status)
end

alias .. "cd .."
alias ... "cd ../.."
alias .... "cd ../../.."
alias ..... "cd ../../../.."
alias ll 'exa -lF --color-scale --no-user --no-time --no-permissions --group-directories-first -a'
alias ls 'exa -F --group-directories-first -a'
alias tree 'tree -C'
alias cat 'bat --paging=never -p'
alias nixos-rebuild-config 'sudo nixos-rebuild switch --cores 2 --upgrade --flake github:shaunsingh/vimrc-dotfiles#shaunsingh-laptop --recreate-lock-file'
alias nixos-rebuild-config-local 'sudo nixos-rebuild switch --cores 2 --upgrade --flake . --recreate-lock-file'
alias neovide "~/IdeaProjects/neovim/neovide/target/release/neovide --multiGrid --frameless"
alias git-rebase 'git rebase -i HEAD~2'
alias update 'brew update; brew upgrade; brew cleanup'
alias cleanup "find . -type f -name '*.DS_Store' -ls -delete"

set -x EDITOR "nvim"
set -x PATH ~/.emacs.d/bin $PATH
set -x PATH /Library/TeX/texbin/ $PATH
set -x PATH ~/.config/scripts $PATH
set PATH $HOME/.cargo/bin $PATH
