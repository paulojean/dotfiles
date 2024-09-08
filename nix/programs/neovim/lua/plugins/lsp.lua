-- add command for organizing imports
vim.api.nvim_create_user_command(
  "OrganizeImports",
  ":call CocAction('runCommand', 'editor.action.organizeImport')",
  { bang = true, nargs = 0, desc = "CoC organize imports" }
)

return {
  {
    "neovim/nvim-lspconfig",
    event = "LazyFile",
    dependencies = {
      "clojure-lsp/clojure-lsp",
    },
    opts = {
      servers = {
        clojure_lsp = {
          mason = false,
        },
        nixd = {
          mason = false,
        },
      },
    },
  },
  { "clojure-lsp/clojure-lsp" },
  { "NoahTheDuke/coc-clojure" },
  {
    "neoclide/coc.nvim",
    event = "LazyFile",
    dependencies = {
      "NoahTheDuke/coc-clojure",
    },
    init = function()
      local wk = require("which-key")
      wk.add({
        { "<space>ci", group = "Imports" },
      })

      vim.api.nvim_set_keymap("n", "<space>cio", "<cmd>OrganizeImports<CR>", { noremap = true })
    end,
  },
}
