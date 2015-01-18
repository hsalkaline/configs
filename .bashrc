export ALTERNATE_EDITOR=""
export EDITOR=emacs

alias a="emacs"

case "$TERM" in
    'xterm') TERM=xterm-256color;;
    'screen') TERM=screen-256color;;
    'Eterm') TERM=Eterm-256color;;
esac
