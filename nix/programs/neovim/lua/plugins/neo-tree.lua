vim.keymap.set("n", "<space>fe", "<cmd>Neotree toggle=true<cr>", { desc = "[F] Toggl[E] Neotree" })

require("neo-tree").setup({
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
})
