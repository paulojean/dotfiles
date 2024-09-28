local neogit = require("neogit")
neogit.setup({})

vim.keymap.set("n", "<leader>gg", "<cmd>Neogit<cr>", { desc = "[G] Neo[G]it" })

require("gitsigns").setup({
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "‾" },
    changedelete = { text = "~" },
  },
  on_attach = function(bufnr)
    local gitsigns = require("gitsigns")

    local function map(mode, l, r, opts)
      opts = opts or {}
      opts.buffer = bufnr
      vim.keymap.set(mode, l, r, opts)
    end

    -- Navigation
    map("n", "]c", function()
      if vim.wo.diff then
        vim.cmd.normal({ "]c", bang = true })
      else
        gitsigns.nav_hunk("next")
      end
    end, { desc = "Jump to next git [c]hange" })

    map("n", "[c", function()
      if vim.wo.diff then
        vim.cmd.normal({ "[c", bang = true })
      else
        gitsigns.nav_hunk("prev")
      end
    end, { desc = "Jump to previous git [c]hange" })

    -- Actions
    -- visual mode
    map("v", "<leader>gs", function()
      gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
    end, { desc = "stage git hunk" })
    map("v", "<leader>gr", function()
      gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
    end, { desc = "reset git hunk" })
    -- normal mode
    map("n", "<leader>gc", require("gitsigns.popup").close, { desc = "[g]it [s]tage hunk" })
    map("n", "<leader>gs", gitsigns.stage_hunk, { desc = "[g]it [s]tage hunk" })
    map("n", "<leader>gr", gitsigns.reset_hunk, { desc = "[g]it [r]eset hunk" })
    map("n", "<leader>gS", gitsigns.stage_buffer, { desc = "[g]it [S]tage buffer" })
    map("n", "<leader>gu", gitsigns.undo_stage_hunk, { desc = "[g]it [u]ndo stage hunk" })
    map("n", "<leader>gR", gitsigns.reset_buffer, { desc = "[g]it [R]eset buffer" })
    map("n", "<leader>gp", gitsigns.preview_hunk, { desc = "[g]it [p]review hunk" })
    map("n", "<leader>gb", gitsigns.blame_line, { desc = "[g]it [b]lame line" })
    map("n", "<leader>gd", gitsigns.diffthis, { desc = "[g]it [d]iff against index" })
    map("n", "<leader>gD", function()
      gitsigns.diffthis("@")
    end, { desc = "[g]it [D]iff against last commit" })
    -- Toggles
    map("n", "<leader>tD", gitsigns.toggle_deleted, { desc = "[t]oggle git show [D]eleted" })
  end,
})
