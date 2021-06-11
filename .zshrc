#latex
export PATH="/Library/TeX/texbin/:$PATH"
#emasc
# PATH="/Applications/Emacs.app/Contents/MacOS:$PATH"
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

