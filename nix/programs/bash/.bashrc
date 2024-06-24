#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
export HISTCONTROL=ignoreboth:erasedups:ignorespace
export HISTSIZE=50000
export HISTFILESIZE=500000
export HISTIGNORE='?:??:git co -:cd -:git ??:git stash:git stash pop'
export LANG=en_US.UTF-8

export HOME=~
export PATH="$HOME/.bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.npm-global/bin:$PATH"
export PATH="$PATH:$HOME/.gem/ruby/2.4.0/bin"
export PATH="$PATH:$HOME/.gem/ruby/2.5.0/bin"
export PATH="$HOME/.config/tlp:$PATH"
export GOPATH=~/.go

# export JAVA_HOME="/Library/Java/JavaVirtualMachines/adoptopenjdk-13.jdk/Contents/Home/"
_jhome=$(readlink -f $(command -v java) | sed "s:bin/java::")
export JAVA_HOME=$_jhome
export PATH=$PATH:$JAVA_HOME/bin
export LEIN_SUPPRESS_USER_LEVEL_REPO_WARNINGS="TRUE"
export PATH="$PATH:$GOPATH/bin"

__clear_screen__() {
  clear && printf '\e[3J'
}

export TERM='xterm-256color'
export EDITOR='emacsclient --alternate-editor "" --tty'

bind -x '"\C-l":__clear_screen__'
bind 'set show-mode-in-prompt on'

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

[ -f ~/.bash_aliases ] && source ~/.bash_aliases
[ -f ~/.bash_helpers ] && source ~/.bash_helpers
[ -f ~/.bash_aws ] && source ~/.bash_aws
[ -f ~/.bash_credentials ] && source ~/.bash_credentials
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
[ -f ~/.suggestions.sh ] && source ~/.suggestions.sh
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
[ -f /usr/local/bin/aws_completer ] && complete -C '/usr/local/bin/aws_completer' aws
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -f ~/.font ] && source ~/.fonts/*.sh
[ -f /usr/local/etc/profile.d/bash-preexec.sh ] && . /usr/local/etc/profile.d/bash-preexec.sh
[ -f ~/.bash-preexec.sh ] && source ~/.bash-preexec.sh

# ps aux | grep '[C]aps' 1>/dev/null || xcape -e 'Caps_Lock=Escape'

export NNN_PLUG='l:launch'
export NNN_COLORS='1234'

preexec() {
  cmd_start="$SECONDS"
}

precmd() {
  last_command_result="$?"

  local WHITE='\[\e[1;37m\]'
  local ORANGE='\[\e[0;33m\]'
  local RED='\[\e[1;31m\]'
  local GREEN='\[\e[1;32m\]'
  local NO_COLLOR='\[\e[0m\]'
  local pretty_command_result

  local cmd_end="$SECONDS"
  elapsed=$((cmd_end-cmd_start))

  if [ "$last_command_result" == "0" ]; then
    pretty_command_result="${GREEN}${NO_COLLOR}"
  else
    pretty_command_result="${RED} [${last_command_result}]${NO_COLLOR}"
  fi

  # do not override PS1 when it has a custom shell
  # this avoids overriding PS1 for applications with custom prompt, eg: nix-shell
  [ -z "$shell" ] && \
    PS1="${WHITE}[\w]${NO_COLLOR}${NO_COLLOR}${ORANGE}$(parse_git_branch)${NO_COLLOR} $(date +%T) ~${elapsed}s ${pretty_command_result} \n~> "
}

export FZF_DEFAULT_COMMAND='ag -s --hidden --ignore .git -g ""'

# FZF_CTRL_T_OPTS="--preview-window wrap --preview '
# if [[ -f {} ]]; then
#     file --mime {} | grep -q \"text\/.*;\" && bat --color \"always\" {} || (tput setaf 1; file --mime {})
# elif [[ -d {} ]]; then
#     exa -l --color always {}
# else
#     tput setaf 1; echo YOU ARE NOT SUPPOSED TO SEE THIS!
# fi'"

PROMPT_DIRTRIM=3

# PS1='\[\e[1;37m\][\w]\[\e[0m\]\[\e[0;36m\]\[\e[0;93m\]$(parse_git_branch)\[\e[0m\]\n~> '
