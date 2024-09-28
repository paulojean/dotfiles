{
  pkgs,
  lib,
  nvim-package,
  nvim-plugins,
  ...
}: let
  build-plugin = name: src:
    pkgs.vimUtils.buildVimPlugin {
      name = name;
      src = src;
    };
  extraPackages = with pkgs; [
    #  LazyVim
    lua-language-server
    stylua

    # luarocks
    lua
    luajitPackages.luarocks

    yaml-language-server
    java-language-server
    bash-language-server

    # Telescope
    ripgrep
    xclip

    # clojure
    clojure-lsp

    # nix
    nixd

    # linter
    clj-kondo

    # formatters
    nixfmt-rfc-style
    alejandra
    cljfmt
    shfmt

    # treesitter
    clang

    lazygit
    nerdfonts
  ];
  extraPackagesPaths = builtins.foldl' (acc: pkg: acc + ":${pkg}/bin") "" extraPackages;
in {
  # https://github.com/nvim-treesitter/nvim-treesitter#i-get-query-error-invalid-node-type-at-position
  xdg.configFile."nvim/parser".source = let
    parsers = pkgs.symlinkJoin {
      name = "treesitter-parsers";
      paths =
        (pkgs.vimPlugins.nvim-treesitter.withPlugins (
          plugins:
            with plugins; [
              bash
              c
              clojure
              comment
              css
              dockerfile
              jq
              json
              lua
              make
              markdown
              nix
              regex
              sql
              terraform
              tsx
              typescript
              yaml
            ]
        ))
        .dependencies;
    };
  in "${parsers}/parser";

  xdg.configFile."nvim/lua".source = ./lua;
  xdg.configFile."clj-kondo/config.edn".text = ''
    {:output {:exclude-files ["conjure-log-.*\\.cljc"]}}
  '';

  xdg.configFile."stylua/stylua.toml".text = ''
    column_width = 120
    line_endings = "Unix"
    indent_type = "Spaces"
    indent_width = 2
    quote_style = "AutoPreferDouble"
    call_parentheses = "Always"
    collapse_simple_statement = "Never"

    [sort_requires]
    enabled = true
  '';

  programs.neovim = {
    enable = true;
    # package = neovim-package;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    # withRuby = false;

    extraPackages = extraPackages;

    plugins = with pkgs.vimPlugins; [
      lazydev-nvim
      luvit-meta
      bufferline-nvim
      cmp-buffer
      cmp-nvim-lsp
      cmp-path
      cmp_luasnip
      conform-nvim
      dashboard-nvim
      dressing-nvim
      flash-nvim
      friendly-snippets
      gitsigns-nvim
      indent-blankline-nvim
      lualine-nvim
      neo-tree-nvim
      neoconf-nvim
      neodev-nvim
      noice-nvim
      nui-nvim
      nvim-cmp
      nvim-lint
      nvim-lspconfig
      # TODO: keep only one notification plugin
      nvim-notify
      fidget-nvim

      nvim-spectre
      (build-plugin "nvim-treesitter" nvim-plugins.nvim-treesitter)
      (build-plugin "nvim-treesitter-context" nvim-plugins.nvim-treesitter-context)
      (build-plugin "nvim-treesitter-textobjects" nvim-plugins.nvim-treesitter-textobjects)
      nvim-ts-autotag
      nvim-ts-context-commentstring
      nvim-web-devicons
      persistence-nvim
      plenary-nvim
      telescope-fzf-native-nvim
      telescope-nvim
      telescope-ui-select-nvim
      todo-comments-nvim
      tokyonight-nvim
      trouble-nvim
      vim-illuminate
      vim-startuptime
      gruvbox-nvim
      dashboard-nvim
      telescope-project-nvim
      telescope-file-browser-nvim

      neogit
      diffview-nvim
      fzf-lua
      toggleterm-nvim

      (build-plugin "nvim-tmux-navigation" nvim-plugins.nvim-tmux-navigation)

      # clojure
      (build-plugin "conjure" nvim-plugins.conjure)
      (build-plugin "cmp-conjure" nvim-plugins.cmp-conjure)
      (build-plugin "nvim-treesitter-sexp" nvim-plugins.nvim-treesitter-sexp)
      (build-plugin "smart-pairs" nvim-plugins.smart-pairs)

      vim-sleuth

      mini-pick

      which-key-nvim

      luasnip

      catppuccin-nvim

      mini-nvim
      (build-plugin "mini-surround" nvim-plugins.mini-surround)
      (build-plugin "mini.ai" nvim-plugins.mini-ai)
    ];

    extraLuaConfig = let
      lua-config = pkgs.substituteAll {
        src = ./init.lua;
        extraPackagesPaths = extraPackagesPaths;
      };
    in
      builtins.readFile lua-config.out;
  };
}
