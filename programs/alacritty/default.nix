{ pkgs, ... }:
{
  programs.alacritty = {
    enable = true;
    settings = {
      env.TERM = "xterm-256color";

      window = {
        dimensions.columns = 80;
        dimensions.lines = 24;

        padding.x = 2;
        padding.y = 2;
      };
      font = {
        size = 12.0;
        normal.family = "FiraCode Nerd Font Mono";
        bold.family = "FiraCode Nerd Font Mono";
        family.family = "FiraCode Nerd Font Mono";
        offset.x = 0;
        offset.y = 0;
        glyph_offset.x = 0;
        glyph_offset.y = 0;
        use_thin_strokes = true;
      };

      custom_cursor_colors = false;

      visual_bell.nimation = "EaseOutExpo";
      visual_bell.duration = 0;

      dynamic_title = true;
      hide_cursor_when_typing = false;
      cursor_style = "Block";
      live_config_reload = true;
    };
  };
}
