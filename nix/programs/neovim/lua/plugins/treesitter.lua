vim.g["conjure#extract#tree_sitter#enabled"] = true
vim.g["conjure#filetype#fennel"] = "conjure.client.fennel.stdio"

require("nvim-treesitter.configs").setup({
  parser_install_dir = TREESITTER_PARSERS_PATH,
  highlight = {
    enable = true,
    disable = function(lang, buf)
      return IS_CONJURE_LOG(buf)
    end,
    -- Some languages depend on vim's regex highlighting system (such as Ruby) for indent rules.
    --  If you are experiencing weird indenting issues, add the language to
    --  the list of additional_vim_regex_highlighting and disabled languages for indent.
    -- additional_vim_regex_highlighting = { 'ruby' },
    additional_vim_regex_highlighting = false,
  },
  indent = {
    enable = true,
    disable = function(lang, buf)
      return IS_CONJURE_LOG(buf)
    end,
  },
  ensure_installed = {},
  auto_install = false,
  modules = {},
  sync_install = false,
  ignore_install = {},
})

vim.opt.runtimepath:prepend(TREESITTER_PARSERS_PATH)
