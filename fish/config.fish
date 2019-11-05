# Config file for fish shell :)


function fish_prompt
    powerline-shell --shell bare $status
end

# List All with Size in a Human readable form and follow symLinks. G is colorized... (fuck)
alias las="ls -lashG"

function show_last_change -d "Shows the diff introduced by the last commit"
	git rev-parse HEAD | git show
end

# Setup python virtualenv and virtualenvwrapper to work properly
set --export VIRTUALFISH_HOME "$HOME/.local/environments"
set --export VIRTUALFISH_DEFAULT_PYTHON "/usr/local/Cellar/python/3.7.0/bin/python3.7"

# Set the default editor to call in all the abbreviations
set --export CLL_EDITOR "emacs"

source ~/.config/fish/abbreviations.fish

# Add my local binaries to the path, I could install things on /usr/local but rather have them in the home 
# directory. Insert it in 0 rather than appending to shadow the system stuff with my own stuff.
set --export PATH $HOME/.local/bin $PATH

eval (python3 -m virtualfish compat_aliases)