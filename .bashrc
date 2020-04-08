#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
export HISTCONTROL=ignoreboth:erasedups
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
export PATH="$PATH:$ANDROID_HOME/tools/emulator"
export PATH="$PATH:$ANDROID_HOME/tools"
export PATH="$PATH:$ANDROID_HOME/platform-tools"
export PATH="$PATH:~/Library/Python/2.7/bin"
export PATH="~/.config/tlp:$PATH"
export PATH="~/watchman:$PATH"
export PATH="$PATH:~/proj"
export PATH="/usr/local/opt/coreutils/libexec/gnubin:/usr/local/bin:$PATH"
export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
export PATH="/usr/local/opt/findutils/libexec/gnubin:$PATH"
export PATH="/usr/local/opt/gnu-tar/libexec/gnubin:$PATH"
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
export KAFKA_HOME="$HOME/.bin/kafka_2.11-2.0.0"
export GOPATH=~/.go
export JAVA_HOME="/Library/Java/JavaVirtualMachines/adoptopenjdk-13.jdk/Contents/Home/"
PATH=$PATH:$JAVA_HOME/bin
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
[ -f ~/.klarnarc ] && source ~/.klarnarc
[ -f ~/.suggestions.sh ] && source ~/.suggestions.sh
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
[ -f /usr/local/bin/aws_completer ] && complete -C '/usr/local/bin/aws_completer' aws
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -f ~/.font ] && source ~/.fonts/*.sh
[ -f /usr/local/etc/profile.d/bash-preexec.sh ] && . /usr/local/etc/profile.d/bash-preexec.sh

source ~/.bash-preexec.sh

preexec() {
  cmd_start="$SECONDS"
}

precmd() {
  last_command_result="$?"
  local cmd_end="$SECONDS"
  elapsed=$((cmd_end-cmd_start))

  # do not override PS1 when it has a custom shell
  # this avoids overriding PS1 for applications with custom prompt, eg: nix-shell
  [ -z "$shell" ] && \
    PS1='\[\e[1;37m\][\w]\[\e[0m\]\[\e[0;36m\]\[\e[0m\]\[\e[0;93m\]$(parse_git_branch)\[\e[0m\] $(date +%T) ~${elapsed}s > $last_command_result \n~> '
}

export FZF_DEFAULT_COMMAND='ag -s --hidden --ignore .git -g ""'

FZF_CTRL_T_OPTS="--preview-window wrap --preview '
if [[ -f {} ]]; then
    file --mime {} | grep -q \"text\/.*;\" && bat --color \"always\" {} || (tput setaf 1; file --mime {})
elif [[ -d {} ]]; then
    exa -l --color always {}
else;
    tput setaf 1; echo YOU ARE NOT SUPPOSED TO SEE THIS!
fi'"

PROMPT_DIRTRIM=3

# PS1='\[\e[1;37m\][\w]\[\e[0m\]\[\e[0;36m\]\[\e[0;93m\]$(parse_git_branch)\[\e[0m\]\n~> '
