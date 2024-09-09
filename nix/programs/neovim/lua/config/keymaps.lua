-- vim.api.nvim_set_keymap("n", "<esc>", ":noh<CR><esc>", { noremap = true })

vim.api.nvim_set_keymap("n", "<C-q>", ":q<CR>", { noremap = true })

vim.keymap.set({ "n", "v" }, '<leader>p', '"_dp', { noremap = true })
vim.keymap.set({ "n", "v" }, '<leader>P', '"_dP', { noremap = true })
vim.keymap.set({ "n", "v" }, '<leader>d', '"_d', { noremap = true })
vim.keymap.set({ "n", "v" }, '<leader>D', '"_D', { noremap = true })
vim.keymap.set({ "n", "v" }, '<leader>C', '"_C', { noremap = true })

-- disable blanklines
vim.keymap.set('n', '<leader>uB', function()
  local enable = not require("ibl.config").get_config(0).enabled

  require("ibl").update({
    enabled = enable
  })
  vim.opt.list = enable
end)
