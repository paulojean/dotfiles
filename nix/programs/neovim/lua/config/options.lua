vim.api.nvim_set_keymap("n", "<esc>", ":noh<CR><esc>", { noremap = true })

-- make whitespaces visible
-- vim.opt.listchars:append({ space = '⋅', tab = '▷⋅', eol = '↵' })
vim.opt.listchars:append({ space = '⋅', tab = '▷⋅' })

vim.g.maplocalleader = ","

return {
  -- add gruvbox
  { "ellisonleao/gruvbox.nvim" },

  -- Configure LazyVim to load gruvbox
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "gruvbox",
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
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
  },
}
