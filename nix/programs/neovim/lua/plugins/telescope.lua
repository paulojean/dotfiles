local telescope = require("telescope")
telescope.setup({
  -- You can put your default mappings / updates / etc. in here
  --  All the info you're looking for is in `:help telescope.setup()`
  extensions = {
    ["ui-select"] = {
      require("telescope.themes").get_dropdown(),
    },
    project = {
      base_dirs = {
        "~/code",
        -- "~/stash",
      },
    },
    file_browser = {},
    undo = {
      use_delta = false,
    },
  },
})
telescope.load_extension("project")
telescope.load_extension("fzf")
telescope.load_extension("undo")
telescope.load_extension("neoclip")

-- Enable Telescope extensions if they are installed
pcall(require("telescope").load_extension, "fzf")
pcall(require("telescope").load_extension, "ui-select")

-- Custom fns

-- We cache the results of "git rev-parse"
-- Process creation is expensive in Windows, so this reduces latency
local is_inside_work_tree = {}

local project_files = function()
  local opts = {} -- define here if you want to define something

  local cwd = vim.fn.getcwd()
  if is_inside_work_tree[cwd] == nil then
    vim.fn.system("git rev-parse --is-inside-work-tree")
    is_inside_work_tree[cwd] = vim.v.shell_error == 0
  end

  if is_inside_work_tree[cwd] then
    require("telescope.builtin").git_files(opts)
  else
    require("telescope.builtin").find_files(opts)
  end
end

local live_grep_from_project_git_root = function()
  local function is_git_repo()
    vim.fn.system("git rev-parse --is-inside-work-tree")

    return vim.v.shell_error == 0
  end

  local function get_git_root()
    local dot_git_path = vim.fn.finddir(".git", ".;")
    return vim.fn.fnamemodify(dot_git_path, ":h")
  end

  local opts = {}

  if is_git_repo() then
    opts = {
      cwd = get_git_root(),
    }
  end

  require("telescope.builtin").live_grep(opts)
end
-- ^ Custon fns

-- See `:help telescope.builtin`
local builtin = require("telescope.builtin")
vim.keymap.set("n", "<leader>sh", builtin.help_tags, { desc = "[s]earch [h]elp" })
vim.keymap.set("n", "<leader>sk", builtin.keymaps, { desc = "[s]earch [k]eymaps" })
vim.keymap.set("n", "<leader>sf", project_files, { desc = "[s]earch [f]iles" })
vim.keymap.set("n", "<leader>ss", builtin.builtin, { desc = "[s]earch [s]elect Telescope" })
vim.keymap.set("n", "<leader>sw", builtin.grep_string, { desc = "[s]earch current [w]ord" })
vim.keymap.set("n", "<leader>sg", live_grep_from_project_git_root, { desc = "[s]earch by [g]rep (only git files)" })
vim.keymap.set("n", "<leader>sG", builtin.live_grep, { desc = "[s]earch by [G]rep" })
vim.keymap.set("n", "<leader>sd", builtin.diagnostics, { desc = "[s]earch [d]iagnostics" })
vim.keymap.set("n", "<leader>sr", builtin.resume, { desc = "[s]earch [r]esume" })
vim.keymap.set("n", "<leader>s.", builtin.oldfiles, { desc = '[s]earch Recent Files ("." for repeat)' })
vim.keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "[ ] Find existing buffers" })

-- Slightly advanced example of overriding default behavior and theme
vim.keymap.set("n", "<leader>sb", function()
  -- You can pass additional configuration to Telescope to change the theme, layout, etc.
  builtin.current_buffer_fuzzy_find(require("telescope.themes").get_dropdown({
    winblend = 10,
    previewer = false,
  }))
end, { desc = "[s]earch, fuzzily, in current [b]uffer" })

-- It's also possible to pass additional configuration options.
--  See `:help telescope.builtin.live_grep()` for information about particular keys
vim.keymap.set("n", "<leader>s/", function()
  builtin.live_grep({
    grep_open_files = true,
    prompt_title = "Live Grep in Open Files",
  })
end, { desc = "[S]earch [/] in Open Files" })

vim.keymap.set("n", "<leader>wp", "<cmd>Telescope project<cr>", { noremap = true, silent = true })

vim.keymap.set(
  "n",
  "<leader>bf",
  "<cmd>Telescope file_browser path=%:p:h select_buffer=true<cr>",
  { noremap = true, silent = true, desc = "[b]rowse [f]iles" }
)

vim.keymap.set(
  "n",
  "<leader>hu",
  "<cmd>Telescope undo<cr>",
  { noremap = true, silent = true, desc = "[h]istory [u]ndo" }
)
