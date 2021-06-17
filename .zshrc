#latex
export PATH="/Library/TeX/texbin/:$PATH"
# doom
PATH="$HOME/.emacs.d/bin:$PATH"
# neovide
alias neovide="/Users/shauryasingh/IdeaProjects/neovim/neovide/target/release/neovide --multiGrid --frameless"
# starship
eval "$(starship init zsh)"
# highlighting
source /opt/homebrew/Cellar/zsh-syntax-highlighting/0.7.1/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# completion
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/shauryasingh/opt/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/shauryasingh/opt/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/shauryasingh/opt/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/shauryasingh/opt/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

