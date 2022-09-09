{ pkgs, ... }:
{
  programs.tmux = {
    enable = true;
    historyLimit = 10000;
    plugins = with pkgs.tmuxPlugins; [
      copycat
      sensible
      gruvbox
      {
        plugin = tmux-fzf;
        extraConfig = ''
          set -g @tmux-fzf-launch-key 'C-f'
        '';
      }
      {
        plugin = prefix-highlight;
        extraConfig = ''
          set -g @prefix_highlight_show_copy_mode 'on'
        '';
      }
      {
        plugin = yank;
        extraConfig = ''
          set -g @custom_copy_command 'my-clipboard-copy --some-arg'
          set -g @yank_action 'copy-pipe'
        '';
      }
    ];

    extraConfig = ''
      unbind C-b
      set -g prefix C-a
      bind-key C-a send-prefix

      set-option -gw xterm-keys on
      set -s escape-time 0
      set -g base-index 1
      set -g default-terminal "screen-256color"
      set -s default-terminal tmux-256color
      set -as terminal-overrides ",*-256color:Tc"
      # set-option -g history-limit 100000
      setw -g mode-keys vi
      # set mouse on
      set -g mode-style bg=colour239,fg=colour223 # colors from theme file
      set -g renumber-windows on

      bind b set status

      bind F resize-pane -Z
      bind S new-session
      bind r source-file ~/.config/tmux/tmux.conf
      bind m command-prompt -p index "move-window -t ':%%'"

      bind \\ split-window -h -c "#{pane_current_path}"
      bind | split-window -fh -c "#{pane_current_path}"
      bind - split-window -v -c "#{pane_current_path}"
      bind _ split-window -fv -c "#{pane_current_path}"
      bind t split-window -fv -p 20 -c "#{pane_current_path}"

      unbind Escape
      bind Escape copy-mode
      bind -T copy-mode-vi v send-keys -X begin-selection
      bind -T copy-mode-vi y send-keys -X copy-selection
      unbind p
      bind p paste-buffer
      bind P choose-buffer

      # Smart pane switching with awareness of vim splits
      is_vim_emacs='echo "#{pane_current_command}" | \
          grep -iqE "((^|\/)g?(view|n?vim?x?)(diff)?$)|emacs"'

      # enable in root key table
      bind -n C-h if-shell "$is_vim_emacs" "send-keys C-h" "select-pane -L"
      bind -n C-j if-shell "$is_vim_emacs" "send-keys C-j" "select-pane -D"
      bind -n C-k if-shell "$is_vim_emacs" "send-keys C-k" "select-pane -U"
      bind -n C-l if-shell "$is_vim_emacs" "send-keys C-l" "select-pane -R"

      bind-key -n M-a choose-session

      bind-key -n M-j switch-client -n
      bind-key -n M-k switch-client -p

      bind-key -n M-n new-window
      bind-key -n M-f resize-pane -Z

      bind-key -n M-h previous-window
      bind-key -n M-l next-window
      bind-key -n M-< swap-window -t -1
      bind-key -n M-> swap-window -t +1

      is_bash='echo "#{pane_current_command}" | \
          grep -iqE "bash"'

      bind l if-shell "$is_bash" "send-keys C-l"

      bind -T copy-mode-vi C-h select-pane -L
      bind -T copy-mode-vi C-j select-pane -D
      bind -T copy-mode-vi C-k select-pane -U
      bind -T copy-mode-vi C-l select-pane -R

      bind -r H resize-pane -L 5
      bind -r J resize-pane -D 5
      bind -r K resize-pane -U 5
      bind -r L resize-pane -R 5

      bind -r C-h select-window -t :-
      bind -r C-l select-window -t :+

      bind -r X kill-pane -a

      # necessary only in Mac, to make copy to clipboard to work
      if-shell "uname | grep -q Darwin" 'set-option -g default-command "reattach-to-user-namespace -l $SHELL"'
    '';
  };
}
