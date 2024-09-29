local lint = require("lint")
lint.linters_by_ft["clojure"] = { "clj-kondo" }
lint.linters_by_ft["nix"] = { "nix" }

-- Create autocommand which carries out the actual linting
-- on the specified events.
local lint_augroup = vim.api.nvim_create_augroup("lint", { clear = true })
vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost" }, {
  group = lint_augroup,
  callback = function()
    if not IS_CONJURE_LOG(0) then
      lint.try_lint()
    end
  end,
})

require("conform").setup({
  notify_on_error = false,
  format_on_save = function(bufnr)
    -- Disable with a global or buffer-local variable
    if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
      return
    end

    return { timeout_ms = 500, lsp_format = "fallback" }
  end,
  formatters_by_ft = {
    ["*"] = { "trim_whitespace", "trim_newlines" },
    lua = { "stylua" },
    clojure = { "cljfmt" },
    fennel = { "fnlfmt" },
    nix = { "alejandra" },
    sh = { "shellcheck" },
    -- You can use 'stop_after_first' to run the first available formatter from the list
    -- javascript = { "prettierd", "prettier", stop_after_first = true },
  },
})

vim.api.nvim_create_user_command("ToggleAutoFormat", function()
  vim.g.disable_autoformat = not vim.g.disable_autoformat
end, {
  desc = "Toggle autoformat-on-save",
})
vim.api.nvim_create_user_command("Format", function(args)
  local range = nil
  if args.count ~= -1 then
    local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
    range = {
      start = { args.line1, 0 },
      ["end"] = { args.line2, end_line:len() },
    }
  end
  require("conform").format({ async = true, lsp_format = "fallback", range = range })
end, { range = true })

vim.keymap.set("n", "<leader>tf", "<cmd>ToggleAutoFormat<cr>", { desc = "[t]oggle [f]ormat on save" })
