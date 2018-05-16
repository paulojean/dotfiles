#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
export HISTCONTROL=ignoreboth:erasedups
export LANG=en_US.UTF-8

export HOME=~
export PATH="$HOME/.bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.npm-global/bin:$PATH"
export PATH="$PATH:$HOME/.gem/ruby/2.4.0/bin"
export PATH="$PATH:$HOME/.gem/ruby/2.5.0/bin"
export ANDROID_HOME=/opt/android-sdk
export PATH="$PATH:$ANDROID_HOME/tools/emulator"
export PATH="$PATH:$ANDROID_HOME/tools"
export PATH="$PATH:$ANDROID_HOME/platform-tools"
export PATH="~/.config/tlp:$PATH"
export PATH="~/watchman:$PATH"
export PATH="$PATH:~/proj"
export KAFKA_HOME="$HOME/.bin/kafka_2.12-0.10.2.1"
export ANDROID_SDK_HOME=/opt/android-sdk
export VISUAL="nvim"
export EDITOR="nvim"
export TERM=xterm-color
export GOPATH="/home/paulo/.go"
export PATH="$PATH:$GOPATH/bin"

if [[ -z "$(ps -aux | grep 'Caps_Lock Escape' | grep xcape)" ]];
then
  setxkbmap -option 'caps:ctrl_modifier'
  xcape -e 'Caps_Lock=Escape'
fi

[ -f ~/.bash_aliases ] && source ~/.bash_aliases
[ -f ~/.bash_helpers ] && source ~/.bash_helpers
[ -f ~/.bash_aws ] && source ~/.bash_aws
[ -f ~/.bash_credentials ] && source ~/.bash_credentials
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
[ -f ~/.suggestions.sh ] && source ~/.suggestions.sh
[ -f ~/.nurc ] && source ~/.nurc

__clear_screen__() {
  clear && printf '\e[3J'
}
bind -x '"\C-l":__clear_screen__'

export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

PROMPT_DIRTRIM=3
PS1='\[\e[1;37m\][\w]\[\e[0m\]\[\e[0;36m\]\[\e[0;93m\]$(parse_git_branch)\[\e[0m\]\$ '
