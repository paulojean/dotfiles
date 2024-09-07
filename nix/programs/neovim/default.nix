{ inputs, pkgs, lib, ... }:
let
  custom-plugins = pkgs.callPackage ./custom-plugins.nix {};
in {
  # https://github.com/nvim-treesitter/nvim-treesitter#i-get-query-error-invalid-node-type-at-position
  xdg.configFile."nvim/parser".source =
    let
      parsers = pkgs.symlinkJoin {
        name = "treesitter-parsers";
        paths = (pkgs.vimPlugins.nvim-treesitter.withPlugins (plugins: with plugins; [
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
        ])).dependencies;
      };
    in
      "${parsers}/parser";

  # Normal LazyVim config here, see https://github.com/LazyVim/starter/tree/main/lua
  xdg.configFile."nvim/lua".source = ./lua;

  programs.neovim = {
    enable = true;
    # package = pkgs.neovim;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    # withRuby = false;

    extraPackages = with pkgs; [
      #  LazyVim
      lua-language-server
      stylua

      # linter
      clj-kondo

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
      custom-plugins.conjure
      custom-plugins.cmp-conjure
      custom-plugins.nvim-treesitter-sexp
      clojure-lsp
      cljfmt

      lazygit

      shfmt
    ];

    plugins = with pkgs.vimPlugins; [
      lazy-nvim
    ];

    extraLuaConfig =
      let
        plugins = with pkgs.vimPlugins; [
          # LazyVim
          lazy-nvim
          LazyVim
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
          nvim-notify
          nvim-spectre
          nvim-treesitter
          nvim-treesitter-context
          nvim-treesitter-textobjects
          nvim-ts-autotag
          nvim-ts-context-commentstring
          nvim-web-devicons
          persistence-nvim
          plenary-nvim
          telescope-fzf-native-nvim
          telescope-nvim
          todo-comments-nvim
          tokyonight-nvim
          trouble-nvim
          vim-illuminate
          vim-startuptime

          custom-plugins.nvim-tmux-navigation

          # clojure
          custom-plugins.conjure
          custom-plugins.cmp-conjure
          custom-plugins.nvim-treesitter-sexp
          coc-nvim
          custom-plugins.coc-clojure

          which-key-nvim
          { name = "LuaSnip"; path = luasnip; }
          { name = "catppuccin"; path = catppuccin-nvim; }
          { name = "mini.ai"; path = mini-nvim; }
          { name = "mini.bufremove"; path = mini-nvim; }
          { name = "mini.comment"; path = mini-nvim; }
          { name = "mini.indentscope"; path = mini-nvim; }
          { name = "mini.pairs"; path = mini-nvim; }
          { name = "mini.surround"; path = mini-nvim; }
        ];
        mkEntryFromDrv = drv:
          if lib.isDerivation drv then
            { name = "${lib.getName drv}"; path = drv; }
          else
            drv;
        lazyPath = pkgs.linkFarm "lazy-plugins" (builtins.map mkEntryFromDrv plugins);
      in
      ''
        vim.opt.rtp:prepend("${lazyPath}/lazy.nvim")
        require("lazy").setup({
          defaults = {
            lazy = true,
          },
          dev = {
            -- reuse files from pkgs.vimPlugins.*
            path = "${lazyPath}",
            patterns = { "." },
            -- fallback to download
            fallback = true,
          },
          spec = {
            { "LazyVim/LazyVim", import = "lazyvim.plugins" },
            -- The following configs are needed for fixing lazyvim on nix
            -- force enable telescope-fzf-native.nvim
            { "nvim-telescope/telescope-fzf-native.nvim", enabled = true },
            -- disable mason.nvim, use programs.neovim.extraPackages
            { "williamboman/mason-lspconfig.nvim", enabled = false },
            { "williamboman/mason.nvim", enabled = false },
            -- import/override with your plugins
            { import = "plugins" },
            -- treesitter handled by xdg.configFile."nvim/parser", put this line at the end of spec to clear ensure_installed
            { "nvim-treesitter/nvim-treesitter",
               opts = function(_, opts)
                opts.ensure_installed = {}
              end,
            },

          },
        })
      '';
  };
}
