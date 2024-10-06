TREESITTER_PARSERS_PATH = "~/.config/nvim"
-- vim.opt.runtimepath:prepend(treesitter_parsers_path)
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
vim.opt.listchars = { tab = "»-", space = "·", trail = "·", nbsp = "␣" }

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

vim.keymap.set({ "v" }, "p", '"_dp', { noremap = true })
vim.keymap.set({ "v" }, "P", '"_dP', { noremap = true })
vim.keymap.set({ "n", "v" }, "D", '"_D', { noremap = true })
vim.keymap.set({ "n", "v" }, "d", '"_d', { noremap = true })
vim.keymap.set({ "n", "v" }, "c", '"_c', { noremap = true })
vim.keymap.set({ "n", "v" }, "C", '"_C', { noremap = true })
vim.keymap.set({ "n", "v" }, "<leader>p", "p", { noremap = true })
vim.keymap.set({ "n", "v" }, "<leader>P", "P", { noremap = true })
vim.keymap.set({ "n", "v" }, "<leader>d", "d", { noremap = true })
vim.keymap.set({ "n", "v" }, "<leader>D", "D", { noremap = true })
vim.keymap.set({ "n", "v" }, "<leader>c", "c", { noremap = true })
vim.keymap.set({ "n", "v" }, "<leader>C", "C", { noremap = true })

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

-- Helper functions
function IS_CONJURE_LOG(buf)
  local bufname = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":t")
  return string.sub(bufname, 1, 12) == "conjure-log-"
end

require("gruvbox").setup({})

vim.o.background = "dark" -- or "light" for light mode
vim.cmd.colorscheme("gruvbox")
-- You can configure highlights by doing something like:
vim.cmd.hi("Comment gui=none")

require("nvim-web-devicons").setup({})

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

local a = 10

require("noice").setup({
  routes = {
    -- hide `written` message
    {
      filter = {
        event = "msg_show",
        kind = "",
        find = "written",
      },
      opts = { skip = true },
    },
  },
  lsp = {
    progress = { enabled = false },
    override = {
      ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
      ["vim.lsp.util.stylize_markdown"] = true,
      ["cmp.entry.get_documentation"] = true, -- requires hrsh7th/nvim-cmp
    },
  },
  presets = {
    bottom_search = false, -- use a classic bottom cmdline for search
    command_palette = true, -- position the cmdline and popupmenu together
    long_message_to_split = true, -- long messages will be sent to a split
    inc_rename = false, -- enables an input dialog for inc-rename.nvim
    lsp_doc_border = false, -- add a border to hover docs and signature help
  },
})

vim.keymap.set("n", "<leader>sna", "<cmd>NoiceAll<cr>", { desc = "[n]oice [a]ll" })
vim.keymap.set("n", "<leader>snd", "<cmd>NoiceDismiss<cr>", { desc = "[n]oice [d]ismiss" })
vim.keymap.set("n", "<leader>snf", "<cmd>NoiceFzf<cr>", { desc = "[n]oice [f]zf" })
vim.keymap.set("n", "<leader>snl", "<cmd>NoiceLast<cr>", { desc = "[n]oice [l]ast message" })

require("plugins.git")

require("ibl").setup({
  exclude = {
    filetypes = { "dashboard" },
  },
})

vim.keymap.set("n", "<leader>tb", function()
  local enable = not require("ibl.config").get_config(0).enabled
  require("ibl").update({
    enabled = enable,
  })
  vim.opt.list = enable
end, { desc = "[T]oggle [B]lanklines" })

WK = require("which-key")

WK.setup({
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
    { "<leader>g", group = "[G]it", mode = { "n", "v" } },
  },
})

vim.keymap.set("n", "<leader>?", function()
  WK.show({ global = false })
end, { desc = "Buffer Local Keymaps (which-key)" })

require("plugins.telescope")

require("plugins.lsp")

require("plugins.lint")

require("todo-comments").setup({ signs = false })

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

require("pairs"):setup({
  enter = {
    enable_mapping = false,
  },
})

local function diff_source()
  local gitsigns = vim.b.gitsigns_status_dict
  if gitsigns then
    return {
      added = gitsigns.added,
      modified = gitsigns.changed,
      removed = gitsigns.removed,
    }
  end
end

require("lualine").setup({
  options = {
    icons_enabled = true,
    theme = "gruvbox_dark",
  },
  sections = {
    lualine_b = { { "diff", source = diff_source } },
    lualine_x = {
      "lsp_progress",
      "encoding",
      "fileformat",
      "filetype",
    },
  },
})

local function is_whitespace(line)
  return vim.fn.match(line, [[^\s*$]]) ~= -1
end

local function all(tbl, check)
  for _, entry in ipairs(tbl) do
    if not check(entry) then
      return false
    end
  end
  return true
end

require("neoclip").setup({
  history = 1000,
  filter = function(data)
    print(vim.inspect(data))
    return not all(data.event.regcontents, is_whitespace)
  end,
  keys = {
    telescope = {
      i = {
        paste = "<c-j>",
        paste_behind = "<c-k>",
      },
    },
  },
})

vim.keymap.set("n", "<leader>hc", "<cmd>Telescope neoclip<cr>", { desc = "[h]istory: [c]lipboard" })

require("plugins.treesitter")

require("plugins.neo-tree")

require("plugins.tmux")

require("toggleterm").setup()
vim.keymap.set({ "n", "i" }, "<c-_>", "<cmd>ToggleTerm<cr>")
