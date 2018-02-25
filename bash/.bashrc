#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
export HISTCONTROL=ignoreboth:erasedups
export LANG=en_US.UTF-8

export HOME=~
export PATH="$HOME/.gem/ruby/2.4.0/bin:$PATH"
export PATH="$HOME/.bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.npm-global/bin:$PATH"
export PATH="$HOME/.gem/ruby/2.4.0/bin:$PATH"
export PATH="$HOME/.gem/ruby/2.5.0/bin:$PATH"
export PATH="$ANDROID_HOME/tools:$PATH"
export PATH="$ANDROID_HOME/platform-tools:$PATH"
export PATH="~/.config/tlp:$PATH"
export PATH="~/watchman:$PATH"
export KAFKA_HOME="$HOME/.bin/kafka_2.12-0.10.2.1"
export ANDROID_SDK_HOME=/opt/android-sdk
export ANDROID_HOME=/opt/android-sdk
export VISUAL="nvim"
export EDITOR="nvim"
export TERM=xterm-color

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
[ -f ~/.nurc ] && source ~/.nurc

export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

PROMPT_DIRTRIM=3
PS1='\[\e[1;37m\][\w]\[\e[0m\]\[\e[0;36m\]\[\e[0;93m\]$(parse_git_branch)\[\e[0m\]\$ '
