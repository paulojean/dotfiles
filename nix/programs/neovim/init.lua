-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are loaded (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Set to true if you have a Nerd Font installed and selected in the terminal
vim.g.have_nerd_font = false

-- [[ Setting options ]]
-- See `:help vim.opt`
-- NOTE: You can change these options as you wish!
--  For more options, you can see `:help option-list`

-- Make line numbers default
vim.opt.number = true
-- You can also add relative line numbers, to help with jumping.
--  Experiment for yourself to see if you like it!
-- vim.opt.relativenumber = true

-- Enable mouse mode, can be useful for resizing splits for example!
vim.opt.mouse = "a"

-- Don't show the mode, since it's already in the status line
vim.opt.showmode = false

-- Sync clipboard between OS and Neovim.
--  Schedule the setting after `UiEnter` because it can increase startup-time.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.schedule(function()
  vim.opt.clipboard = "unnamedplus"
end)

-- Enable break indent
vim.opt.breakindent = true

-- Save undo history
vim.opt.undofile = true

-- Case-insensitive searching UNLESS \C or one or more capital letters in the search term
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Keep signcolumn on by default
vim.opt.signcolumn = "yes"

-- Decrease update time
vim.opt.updatetime = 250

-- Decrease mapped sequence wait time
-- Displays which-key popup sooner
vim.opt.timeoutlen = 300

-- Configure how new splits should be opened
vim.opt.splitright = true
vim.opt.splitbelow = true

-- Sets how neovim will display certain whitespace characters in the editor.
--  See `:help 'list'`
--  and `:help 'listchars'`
vim.opt.list = true
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }

-- Preview substitutions live, as you type!
vim.opt.inccommand = "split"

-- Show which line your cursor is on
vim.opt.cursorline = true

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 5

vim.opt.colorcolumn = "80"

-- [[ Basic Keymaps ]]
--  See `:help vim.keymap.set()`

-- Clear highlights on search when pressing <Esc> in normal mode
--  See `:help hlsearch`
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Diagnostic keymaps
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })

vim.api.nvim_set_keymap("n", "<C-s>", ":w<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "<C-q>", ":q<CR>", { noremap = true })

vim.keymap.set({ "n", "v" }, "<leader>p", '"_dp', { noremap = true })
vim.keymap.set({ "n", "v" }, "<leader>P", '"_dP', { noremap = true })
vim.keymap.set({ "n", "v" }, "<leader>d", '"_d', { noremap = true })
vim.keymap.set({ "n", "v" }, "<leader>D", '"_D', { noremap = true })
vim.keymap.set({ "n", "v" }, "<leader>C", '"_C', { noremap = true })

-- Exit terminal mode in the builtin terminal with a shortcut that is a bit easier
-- for people to discover. Otherwise, you normally need to press <C-\><C-n>, which
-- is not what someone will guess without a bit more experience.
--
-- NOTE: This won't work in all terminal emulators/tmux/etc. Try your own mapping
-- or just use <C-\><C-n> to exit terminal mode
vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

-- Keybinds to make split navigation easier.
--  Use CTRL+<hjkl> to switch between windows
--
--  See `:help wincmd` for a list of all window commands
vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" })
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" })

vim.keymap.set("n", "[b", "<cmd>bprevious<cr>", { desc = "Move to previous buffer" })
vim.keymap.set("n", "]b", "<cmd>bnext<cr>", { desc = "Move to previous buffer" })

vim.keymap.set("n", "<leader>vh", "<cmd>split<cr>", { desc = "Split window [H]orizontally" })
vim.keymap.set("n", "<leader>vs", "<cmd>vsplit<cr>", { desc = "Split window [V]ertically" })

vim.keymap.set("n", "<leader>wt", "<cmd>tabnew<cr>", { desc = "[W] New [T]ab" })

-- [[ Basic Autocommands ]]
--  See `:help lua-guide-autocommands`

-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.highlight.on_yank()`
vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- @extraPackagesPaths@ is provided by `./default.nix`
vim.env.PATH = vim.env.PATH .. "@extraPackagesPaths@"

-- @lazyPath@ is provided by `./default.nix`
local lazyPath = "@lazyPath@"
vim.opt.rtp:prepend(lazyPath .. "/lazy.nvim")
-- TODO: remove, lazy is imported in `default.nix`
-- -- [[ Install `lazy.nvim` plugin manager ]]
-- --    See `:help lazy.nvim.txt` or https://github.com/folke/lazy.nvim for more info
-- local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
-- if not (vim.uv or vim.loop).fs_stat(lazypath) then
--   local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
--   local out = vim.fn.system { 'git', 'clone', '--filter=blob:none', '--branch=stable', lazyrepo, lazypath }
--   if vim.v.shell_error ~= 0 then
--     error('Error cloning lazy.nvim:\n' .. out)
--   end
-- end ---@diagnostic disable-next-line: undefined-field
-- vim.opt.rtp:prepend(lazypath)

-- [[ Configure and install plugins ]]
--
--  To check the current status of your plugins, run
--    :Lazy
--
--  You can press `?` in this menu for help. Use `:q` to close the window
--
--  To update plugins you can run
--    :Lazy update
--
-- NOTE: Here is where you install your plugins.

-- Helper functions
function is_conjure_log(buf)
  local bufname = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":t")
  return string.sub(bufname, 1, 12) == "conjure-log-"
end

require("lazy").setup({
  defaults = {
    lazy = true,
  },
  dev = {
    -- reuse files from pkgs.vimPlugins.*
    path = lazyPath,
    patterns = { "." },
    -- fallback to download
    fallback = true,
  },
  spec = {
    -- NOTE: Plugins can be added with a link (or for a github repo: 'owner/repo' link).
    {
      "tpope/vim-sleuth", -- Detect tabstop and shiftwidth automatically
      event = "VeryLazy",
    },

    -- NOTE: Plugins can also be added by using a table,
    -- with the first argument being the link and the following
    -- keys can be used to configure plugin behavior/loading/etc.
    --
    -- Use `opts = {}` to force a plugin to be loaded.
    --

    -- Here is a more advanced example where we pass configuration
    -- options to `gitsigns.nvim`. This is equivalent to the following Lua:
    --    require('gitsigns').setup({ ... })
    --
    -- See `:help gitsigns` to understand what the configuration keys do
    -- source: https://github.com/nvim-lua/kickstart.nvim/blob/master/lua/kickstart/plugins/gitsigns.lua
    ---
    -- Misc
    ---
    require("plugins.git"),
    {
      "lukas-reineke/indent-blankline.nvim",
      event = "VeryLazy",
      main = "ibl",
      ---@module "ibl"
      ---@type ibl.config
      opts = {
        exclude = {
          filetypes = { "dashboard" },
        },
      },
      init = function()
        -- toggle blanklines
        vim.keymap.set("n", "<leader>tB", function()
          local enable = not require("ibl.config").get_config(0).enabled
          require("ibl").update({
            enabled = enable,
          })
          vim.opt.list = enable
        end, { desc = "[T]oggle [B]lanklines" })
      end,
    },

    -- NOTE: Plugins can also be configured to run Lua code when they are loaded.
    --
    -- This is often very useful to both group configuration, as well as handle
    -- lazy loading plugins that don't need to be loaded immediately at startup.
    --
    -- For example, in the following configuration, we use:
    --  event = 'VimEnter'
    --
    -- which loads which-key before all the UI elements are loaded. Events can be
    -- normal autocommands events (`:help autocmd-events`).
    --
    -- Then, because we use the `config` key, the configuration only runs
    -- after the plugin has been loaded:
    --  config = function() ... end

    { -- Useful plugin to show you pending keybinds.
      "folke/which-key.nvim",
      event = "VimEnter", -- Sets the loading event to 'VimEnter'
      opts = {
        icons = {
          -- set icon mappings to true if you have a Nerd Font
          mappings = vim.g.have_nerd_font,
          -- If you are using a Nerd Font: set icons.keys to an empty table which will use the
          -- default whick-key.nvim defined Nerd Font icons, otherwise define a string table
          keys = vim.g.have_nerd_font and {} or {
            Up = "<Up> ",
            Down = "<Down> ",
            Left = "<Left> ",
            Right = "<Right> ",
            C = "<C-…> ",
            M = "<M-…> ",
            D = "<D-…> ",
            S = "<S-…> ",
            CR = "<CR> ",
            Esc = "<Esc> ",
            ScrollWheelDown = "<ScrollWheelDown> ",
            ScrollWheelUp = "<ScrollWheelUp> ",
            NL = "<NL> ",
            BS = "<BS> ",
            Space = "<Space> ",
            Tab = "<Tab> ",
            F1 = "<F1>",
            F2 = "<F2>",
            F3 = "<F3>",
            F4 = "<F4>",
            F5 = "<F5>",
            F6 = "<F6>",
            F7 = "<F7>",
            F8 = "<F8>",
            F9 = "<F9>",
            F10 = "<F10>",
            F11 = "<F11>",
            F12 = "<F12>",
          },
        },

        -- Document existing key chains
        spec = {
          { "<leader>c", group = "[C]ode", mode = { "n", "x" } },
          { "<leader>d", group = "[D]ocument" },
          { "<leader>r", group = "[R]ename" },
          { "<leader>s", group = "[S]earch" },
          { "<leader>w", group = "[W]orkspace" },
          { "<leader>t", group = "[T]oggle" },
          { "<leader>v", group = "[V]iew" },
          { "<leader>h", group = "Git [H]unk", mode = { "n", "v" } },
        },
      },
    },

    -- NOTE: Plugins can specify dependencies.
    --
    -- The dependencies are proper plugin specifications as well - anything
    -- you do for a plugin at the top level, you can do for a dependency.
    --
    -- Use the `dependencies` key to specify the dependencies of a particular plugin

    require("plugins.telescope"),
    -- LSP Plugins
    require("plugins.lsp"),
    ---
    -- Formatting
    ---
    require("plugins.lint"),
    { -- Autocompletion
      "hrsh7th/nvim-cmp",
      event = "InsertEnter",
      dependencies = {
        -- Snippet Engine & its associated nvim-cmp source
        {
          "L3MON4D3/LuaSnip",
          dependencies = {
            -- `friendly-snippets` contains a variety of premade snippets.
            --    See the README about individual language/framework/plugin snippets:
            --    https://github.com/rafamadriz/friendly-snippets
          },
        },
        "saadparwaiz1/cmp_luasnip",

        -- Adds other completion capabilities.
        --  nvim-cmp does not ship with all sources by default. They are split
        --  into multiple repos for maintenance purposes.
        "hrsh7th/cmp-nvim-lsp",
        "hrsh7th/cmp-path",
      },
      config = function()
        -- See `:help cmp`
        local cmp = require("cmp")
        local luasnip = require("luasnip")
        luasnip.config.setup({})

        cmp.setup({
          snippet = {
            expand = function(args)
              luasnip.lsp_expand(args.body)
            end,
          },
          completion = { completeopt = "menu,menuone,noinsert" },

          -- For an understanding of why these mappings were
          -- chosen, you will need to read `:help ins-completion`
          --
          -- No, but seriously. Please read `:help ins-completion`, it is really good!
          mapping = cmp.mapping.preset.insert({
            -- Select the [n]ext item
            ["<C-n>"] = cmp.mapping.select_next_item(),
            -- Select the [p]revious item
            ["<C-p>"] = cmp.mapping.select_prev_item(),

            -- Scroll the documentation window [b]ack / [f]orward
            ["<C-b>"] = cmp.mapping.scroll_docs(-4),
            ["<C-f>"] = cmp.mapping.scroll_docs(4),

            -- Accept ([y]es) the completion.
            --  This will auto-import if your LSP supports it.
            --  This will expand snippets if the LSP sent a snippet.
            ["<C-y>"] = cmp.mapping.confirm({ select = true }),

            -- If you prefer more traditional completion keymaps,
            -- you can uncomment the following lines
            --['<CR>'] = cmp.mapping.confirm { select = true },
            --['<Tab>'] = cmp.mapping.select_next_item(),
            --['<S-Tab>'] = cmp.mapping.select_prev_item(),

            -- Manually trigger a completion from nvim-cmp.
            --  Generally you don't need this, because nvim-cmp will display
            --  completions whenever it has completion options available.
            ["<C-Space>"] = cmp.mapping.complete({}),

            -- Think of <c-l> as moving to the right of your snippet expansion.
            --  So if you have a snippet that's like:
            --  function $name($args)
            --    $body
            --  end
            --
            -- <c-l> will move you to the right of each of the expansion locations.
            -- <c-h> is similar, except moving you backwards.
            ["<C-l>"] = cmp.mapping(function()
              if luasnip.expand_or_locally_jumpable() then
                luasnip.expand_or_jump()
              end
            end, { "i", "s" }),
            ["<C-h>"] = cmp.mapping(function()
              if luasnip.locally_jumpable(-1) then
                luasnip.jump(-1)
              end
            end, { "i", "s" }),

            -- For more advanced Luasnip keymaps (e.g. selecting choice nodes, expansion) see:
            --    https://github.com/L3MON4D3/LuaSnip?tab=readme-ov-file#keymaps
          }),
          sources = {
            {
              name = "lazydev",
              -- set group index to 0 to skip loading LuaLS completions as lazydev recommends it
              group_index = 0,
            },
            { name = "nvim_lsp" },
            { name = "luasnip" },
            { name = "path" },
            { name = "conjure" },
          },
        })
      end,
    },
    {
      "nvimdev/dashboard-nvim",
      event = "VimEnter",
      config = function()
        require("dashboard").setup({
          config = {
            week_header = { enable = true },
            shortcut = {
              {
                desc = "[R]ecent files",
                group = "@project",
                key = "r",
                action = "Telescope oldfiles",
              },
              {
                desc = "[F]ind files",
                group = "@project",
                key = "f",
                action = "Telescope find_files",
              },
              {
                desc = "[G]rep current files",
                group = "@project",
                key = "g",
                action = "Telescope live_grep",
              },
              {
                desc = "[Q]uit Nvim",
                group = "@misc",
                key = "q",
                action = "qa",
              },
            },
          },
        })
      end,
    },
    { -- You can easily change to a different colorscheme.
      -- Change the name of the colorscheme plugin below, and then
      -- change the command in the config to whatever the name of that colorscheme is.
      --
      -- If you want to see what colorschemes are already installed, you can use `:Telescope colorscheme`.
      "ellisonleao/gruvbox.nvim",
      priority = 1000, -- Make sure to load this before all the other start plugins.
      init = function()
        -- Load the colorscheme here.
        -- Like many other themes, this one has different styles, and you could load
        -- any other, such as 'tokyonight-storm', 'tokyonight-moon', or 'tokyonight-day'.
        vim.o.background = "dark"
        vim.cmd.colorscheme("gruvbox")

        -- You can configure highlights by doing something like:
        vim.cmd.hi("Comment gui=none")
      end,
    },
    {
      -- required by: telescope, dashboard, neo-tree
      "nvim-tree/nvim-web-devicons",
      priority = 1000,
      enabled = true,
      -- enabled = vim.g.have_nerd_font,
    },

    -- Highlight todo, notes, etc in comments
    {
      "folke/todo-comments.nvim",
      event = "VimEnter",
      dependencies = { "nvim-lua/plenary.nvim" },
      opts = { signs = false },
    },
    { -- Collection of various small independent plugins/modules
      "echasnovski/mini.nvim",
      event = "VimEnter",
      config = function()
        -- Better Around/Inside textobjects
        --
        -- Examples:
        --  - va)  - [V]isually select [A]round [)]paren
        --  - yinq - [Y]ank [I]nside [N]ext [Q]uote
        --  - ci'  - [C]hange [I]nside [']quote
        require("mini.ai").setup({ n_lines = 500 })

        -- Add/delete/replace surroundings (brackets, quotes, etc.)
        -- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
        -- - sd'   - [S]urround [D]elete [']quotes
        -- - sr)'  - [S]urround [R]eplace [)] [']
        require("mini.surround").setup()

        -- Simple and easy statusline.
        --  You could remove this setup call if you don't like it,
        --  and try some other statusline plugin
        local statusline = require("mini.statusline")
        -- set use_icons to true if you have a Nerd Font
        statusline.setup({ use_icons = true, set_vim_settings = true })

        -- You can configure sections in the statusline by overriding their
        -- default behavior. For example, here we set the section for
        -- cursor location to LINE:COLUMN
        ---@diagnostic disable-next-line: duplicate-set-field
        statusline.section_location = function()
          return "%2l:%-2v"
        end

        -- ... and there is more!
        --  Check out: https://github.com/echasnovski/mini.nvim
      end,
    },
    { -- Highlight, edit, and navigate code
      "nvim-treesitter/nvim-treesitter",
      event = "VeryLazy",
      main = "nvim-treesitter.configs", -- Sets main module to use for opts
      -- [[ Configure Treesitter ]] See `:help nvim-treesitter`
      opts = {
        ensure_installed = {},
        highlight = {
          enable = true,
          disable = function(lang, buf)
            return is_conjure_log(buf)
          end,
          -- Some languages depend on vim's regex highlighting system (such as Ruby) for indent rules.
          --  If you are experiencing weird indenting issues, add the language to
          --  the list of additional_vim_regex_highlighting and disabled languages for indent.
          -- additional_vim_regex_highlighting = { 'ruby' },
        },
        indent = {
          enable = true,
          disable = function(lang, buf)
            return is_conjure_log(buf)
          end,
        },
      },
      config = function(_, opts)
        vim.list_extend(opts.ensure_installed, {
          "bash",
          "clojure",
          "diff",
          "html",
          "javascript",
          "json",
          "lua",
          "markdown",
          "markdown_inline",
          "python",
          "query",
          "regex",
          "tsx",
          "typescript",
          "typescript",
          "vim",
          "yaml",
        })
      end,
      -- There are additional nvim-treesitter modules that you can use to interact
      -- with nvim-treesitter. You should go explore a few and see what interests you:
      --
      --    - Incremental selection: Included, see `:help nvim-treesitter-incremental-selection-mod`
      --    - Show your current context: https://github.com/nvim-treesitter/nvim-treesitter-context
      --    - Treesitter + textobjects: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
    },

    -- The following two comments only work if you have downloaded the kickstart repo, not just copy pasted the
    -- init.lua. If you want these files, they are in the repository, so you can just download them and
    -- place them in the correct locations.

    -- NOTE: Next step on your Neovim journey: Add/Configure additional plugins for Kickstart
    --
    --  Here are some example plugins that I've included in the Kickstart repository.
    --  Uncomment any of the lines below to enable them (you will need to restart nvim).
    --
    -- require 'kickstart.plugins.debug',
    -- require 'kickstart.plugins.indent_line',
    -- require 'kickstart.plugins.lint',
    -- require 'kickstart.plugins.autopairs',
    -- require 'kickstart.plugins.neo-tree',
    -- require 'kickstart.plugins.gitsigns', -- adds gitsigns recommend keymaps

    -- NOTE: The import below can automatically add your own plugins, configuration, etc from `lua/custom/plugins/*.lua`
    --    This is the easiest way to modularize your config.
    --
    --  Uncomment the following line and add your plugins to `lua/custom/plugins/*.lua` to get going.
    --    For additional information, see `:help lazy.nvim-lazy.nvim-structuring-your-plugins`
    -- { import = 'custom.plugins' },
    ---
    -- Navigation
    ---
    {
      "nvim-neo-tree/neo-tree.nvim",
      event = "VeryLazy",
      keys = {
        { "<space>fe", "<cmd>Neotree toggle=true<cr>", { desc = "[F] Toggl[E] Neotree" } },
      },
      dependencies = {
        "MunifTanjim/nui.nvim",
      },
      opts = {
        filesystem = {
          filtered_items = {
            visible = true,
            hide_by_name = {
              ".git",
              ".DS_Store",
              "thumbs.db",
              "node_modules",
            },
            never_show = { ".git" },
          },
          follow_current_file = {
            enabled = true,
          },
        },
        window = {
          mappings = {
            ["l"] = "open",
            ["<cr>"] = "focus_preview",
          },
        },
      },
    },
    {
      "christoomey/vim-tmux-navigator",
      event = "VeryLazy",
      cmd = {
        "TmuxNavigateLeft",
        "TmuxNavigateDown",
        "TmuxNavigateUp",
        "TmuxNavigateRight",
        "TmuxNavigatePrevious",
      },
      keys = {
        { "<c-h>", "<cmd><C-U>TmuxNavigateLeft<cr>" },
        { "<c-j>", "<cmd><C-U>TmuxNavigateDown<cr>" },
        { "<c-k>", "<cmd><C-U>TmuxNavigateUp<cr>" },
        { "<c-l>", "<cmd><C-U>TmuxNavigateRight<cr>" },
        { "<c-\\>", "<cmd><C-U>TmuxNavigatePrevious<cr>" },
      },
    },
    {
      "akinsho/toggleterm.nvim",
      event = "VeryLazy",
      config = true,
      keys = {
        --  c-/
        { "<c-_>", "<cmd>ToggleTerm<cr>" },
      },
    },

    ---
    -- Coding
    ---

    -- clojure
    {
      "Olical/conjure",
      event = "VeryLazy",
      lazy = true,
      dependencies = {
        "PaterJason/cmp-conjure",
      },
      init = function()
        -- prefer LSP for jump-to-definition and symbol-doc, and use conjure
        -- alternatives with <localleader>K and <localleader>gd
        vim.g["conjure#mapping#doc_word"] = "K"
        vim.g["conjure#mapping#def_word"] = "gd"
      end,
    },
    {
      "PaterJason/cmp-conjure",
      -- event = "LazyFile",
      event = "VeryLazy",
      config = function()
        local cmp = require("cmp")
        local config = cmp.get_config()
        table.insert(config.sources, { name = "conjure" })
        return cmp.setup(config)
      end,
    },
    {
      "PaterJason/nvim-treesitter-sexp",
      opts = {},
      event = "VeryLazy",
    },
  },
  {
    ui = {
      -- If you are using a Nerd Font: set icons to an empty table which will use the
      -- default lazy.nvim defined Nerd Font icons, otherwise define a unicode icons table
      icons = vim.g.have_nerd_font and {} or {
        cmd = "⌘",
        config = "🛠",
        event = "📅",
        ft = "📂",
        init = "⚙",
        keys = "🗝",
        plugin = "🔌",
        runtime = "💻",
        require = "🌙",
        source = "📄",
        start = "🚀",
        task = "📌",
        lazy = "💤 ",
      },
    },
  },
})

vim.api.nvim_set_keymap("n", "n", "n", { noremap = true })

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
