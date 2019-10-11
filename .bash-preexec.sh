preexec() {
  cmd_start="$SECONDS"
}

precmd() {
  local cmd_end="$SECONDS"
  elapsed=$((cmd_end-cmd_start))
  PS1='\[\e[1;37m\][\w]\[\e[0m\]\[\e[0;36m\]\[\e[0m\]\[\e[0;93m\]$(parse_git_branch)\[\e[0m\] ~${elapsed}s \n~> '
}
