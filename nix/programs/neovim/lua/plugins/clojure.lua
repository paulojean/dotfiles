return {
  {
    "Olical/conjure",
    event = "LazyFile",
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
    lazy = true,
    config = function()
      local cmp = require("cmp")
      local config = cmp.get_config()
      table.insert(config.sources, { name = "conjure" })
      return cmp.setup(config)
    end,
  },
  { "PaterJason/nvim-treesitter-sexp" },
  { "venantius/vim-cljfmt" },
  {
    "hrsh7th/nvim-cmp",
    optional = true,
    dependencies = {
      "PaterJason/cmp-conjure",
    },
    opts = function(_, opts)
      if type(opts.sources) == "table" then
        vim.list_extend(opts.sources, { name = "clojure" })
      end
    end,
  },
  { "PaterJason/nvim-treesitter-sexp", opts = {}, event = "LazyFile" },
}
