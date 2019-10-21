# -*- sh -*-

alias ls='exa --group-directories-first'
alias ll='exa -la --group-directories-first'
alias lt='exa -lT --git-ignore'
alias adbsi="adb shell input text"
alias ipup="sudo ip link set wlp2s0 up"
alias g="git"
alias vi=$(which nvim)
alias n=$(which nvim)
alias vim='emacsclient --alternate-editor "" --tty'
alias e='emacsclient --alternate-editor "" --tty'
alias die="shutdown now"
alias fuck='sudo "$SHELL" -c "$(history -p !!)"'
alias dev='tmux -2 new-session -A -s stuffs'
alias pipup='pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U'
alias pipup3='pip3.7 list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip3.7 install -U'
alias du="ncdu --color dark -rr -x --exclude .git --exclude node_modules"

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
