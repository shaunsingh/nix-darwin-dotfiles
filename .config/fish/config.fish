# make sure nix works with fish
set fish_function_path $fish_function_path ~/.config/fish/plugin-foreign-env/functions
fenv source ~/.nix-profile/etc/profile.d/nix.sh

# add nessecary vterm features
function vterm_printf;
    if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

if [ "$INSIDE_EMACS" = 'vterm' ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    end
end

function fish_title
    hostname
    echo ":"
    pwd
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

set __fish_git_prompt_char_dirtystate '~~'
set __fish_git_prompt_char_stateseparator ' '
set __fish_git_prompt_char_untrackedfiles ' ...'
set __fish_git_prompt_char_cleanstate '✓'
set __fish_git_prompt_char_stagedstate '-> '
set __fish_git_prompt_char_conflictedstate "✕"

set __fish_git_prompt_char_upstream_prefix ''
set __fish_git_prompt_char_upstream_equal ''
set __fish_git_prompt_char_upstream_ahead '>>='
set __fish_git_prompt_char_upstream_behind '=<<'
set __fish_git_prompt_char_upstream_diverged '<=>'

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

    if test $HOME != $PWD
        _print_in_color ""(prompt_pwd) blue
    end
    __fish_git_prompt " (%s)"

    _print_in_color " λ " (_prompt_color_for_status $last_status)
end

set -x EDITOR "nvim"
set -x PATH ~/.emacs.d/bin $PATH
set -x PATH /Applications/Emacs.app/Contents/MacOS $PATH
set -x PATH ~/.config/scripts $PATH

alias vi "emacsclient -c --alternate-editor=COMMAND"
alias .. "cd .."
alias ... "cd ../.."
alias .... "cd ../../.."
alias ..... "cd ../../../.."
alias git-rebase 'git rebase -i HEAD~2'
alias ll 'exa -lF --color-scale --no-user --no-time --no-permissions --group-directories-first --icons -a'
alias ls 'exa -F --group-directories-first --icons -a'
alias tree 'tree -a -C'
alias cat 'bat --paging=never -p'
alias cleanup "find . -type f -name '*.DS_Store' -ls -delete && rm -r ~/.config/discord && rm -r ~/.config/GIMP && rm -r ~/.config/chromium "

