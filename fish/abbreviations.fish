abbr --add e $CLL_EDITOR
abbr --add cfg $CLL_EDITOR ~/.config/fish/config.fish
abbr --add fv2u copy_fusionv_binary_to_unitylibs
abbr --add abcfg $CLL_EDITOR ~/.config/fish/abbreviations.fish
abbr --add pwcfg $CLL_EDITOR ~/.config/powerline-shell/config.json
abbr --add g cd ~/git
abbr --add gs git status
abbr --add gd git diff
abbr --add ga git add
abbr --add gc git commit
abbr --add gl git log

# Check out a git file
abbr --add gch git checkout HEAD 
# Check out all the json files (useful in the stupid FSM thingy)
abbr --add gcj "git checkout head Assets/**.json"

# Weird encantation to find all the modified files asset bundles
abbr --add gab "git status --porcelain=v1 | ack .meta --no-color | awk -F ' ' '{print \$2}' | xargs ack -iH 'assetbundle'"