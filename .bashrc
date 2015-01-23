export ALTERNATE_EDITOR=""
export EDITOR="emacs --no-desktop"

alias e="emacs"

case "$TERM" in
    'xterm') TERM=xterm-256color;;
    'screen') TERM=screen-256color;;
    'Eterm') TERM=Eterm-256color;;
esac

export PATH=~/bin:$PATH
