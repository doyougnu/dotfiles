# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/doyougnu/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

alias hg='history | grep'

function lookup () { sdcv $1 | less }
function _lookup () { sdcv $1 }

function lookup-notify () { notify-send -t 2000 "$(lookup $1)" }

bindkey "^k" history-beginning-search-backward
bindkey "^l" history-beginning-search-forward
bindkey -v "^p" up-line-or-history
bindkey -v "^n" down-line-or-history
bindkey -v "fd" vi-cmd-mode
bindkey -v "^j" autosuggest-accept
bindkey -v "^ " autosuggest-execute

heroku(){
  docker run -it --rm -u $(id -u):$(id -g) -w "$HOME" \
    -v /etc/passwd:/etc/passwd:ro \
    -v /etc/group:/etc/group:ro \
    -v /etc/localtime:/etc/localtime:ro \
    -v /home:/home \
    -v /tmp:/tmp \
    -v /run/user/$(id -u):/run/user/$(id -u) \
    -v $(pwd):/workdir \
    -w /workdir \
    --name heroku \
    johnnagro/heroku-toolbelt "$@"
}
