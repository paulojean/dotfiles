preexec() {
  cmd_start="$SECONDS"
}

precmd() {
  last_command_result="$?"
  local cmd_end="$SECONDS"
  elapsed=$((cmd_end-cmd_start))

  # only override PS1 when it has the default value
  # this avoids overriding PS1 for applications with custom prompt, eg: nix-shell
  [ "`echo $PS1`" == '\h:\W \u\$' ] && \
    PS1='\[\e[1;37m\][\w]\[\e[0m\]\[\e[0;36m\]\[\e[0m\]\[\e[0;93m\]$(parse_git_branch)\[\e[0m\] \@ $(date +%T) ~${elapsed}s > $last_command_result \n~> '
}
