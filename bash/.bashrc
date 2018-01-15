#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
export HISTCONTROL=ignoreboth:erasedups
export LANG=en_US.UTF-8

export NU_HOME=~/nu
export NUCLI_HOME="$NU_HOME/nucli"
export PATH="$NUCLI_HOME:$PATH"

export HOME=~
export PATH="$HOME/.gem/ruby/2.4.0/bin:$PATH"
export PATH="$HOME/.bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.npm-global/bin:$PATH"
export PATH="$HOME/.gem/ruby/2.4.0/bin:$PATH"
export PATH="$HOME/.gem/ruby/2.5.0/bin:$PATH"
export ANDROID_HOME=/opt/android-sdk
export PATH="$ANDROID_HOME/tools:$PATH"
export PATH="$ANDROID_HOME/platform-tools:$PATH"
export KAFKA_HOME="$HOME/.bin/kafka_2.12-0.10.2.1"

export PATH="~/.config/tlp:$PATH"
export PATH="~/watchman:$PATH"
export VISUAL="nvim"
export EDITOR="nvim"
export TERM=xterm-color

[ -f ~/.bash_aliases ] && source ~/.bash_aliases
[ -f ~/.bash_helpers ] && source ~/.bash_helpers
[ -f ~/.bash_aws ] && source ~/.bash_aws
[ -f ~/.bash_credentials ] && source ~/.bash_credentials
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

PROMPT_DIRTRIM=3
#export PS1='$ \u in \w: '
PS1='\[\e[0;36m\]┌─\[\e[1;37m\][\w]\[\e[0m\]\[\e[0;36m\]\[\e[0;93m\]$(parse_git_branch)\n\[\e[0;36m\]└─\[\e[1;32m\][\A]\[\e[0m\]\$ '

#export MYPS='$(echo -n "${PWD/#$HOME/~}" | awk -F "/" '"'"'{
#if (length($0) > 14) { if (NF>4) print $1 "/" $2 "/.../" $(NF-1) "/" $NF;
#else if (NF>3) print $1 "/" $2 "/.../" $NF;
#else print $1 "/.../" $NF; }
#else print $0;}'"'"')'
#PS1='$(eval "echo ${MYPS}")\[\e[0;93m\]$(parse_git_branch)$ '
