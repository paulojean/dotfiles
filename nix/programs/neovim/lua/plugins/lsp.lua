require("lazydev").setup({
  library = {
    -- Load luvit types when the `vim.uv` word is found
    { path = "luvit-meta/library", words = { "vim%.uv" } },
  },
})

require("fidget").setup({})

local luasnip = require("luasnip")

cmp = require("cmp")
cmp.setup({
  sources = {
    {
      name = "lazydev",
      -- set group index to 0 to skip loading LuaLS completions as lazydev recommends it
      group_index = 0,
    },
    { name = "nvim_lsp" },
    { name = "luasnip" },
    { name = "path" },
    { name = "conjure" },
  },
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  completion = { completeopt = "menu,menuone,noinsert" },
  mapping = cmp.mapping.preset.insert({
    -- Select the [n]ext item
    ["<C-n>"] = cmp.mapping.select_next_item(),
    -- Select the [p]revious item
    ["<C-p>"] = cmp.mapping.select_prev_item(),

    -- Scroll the documentation window [b]ack / [f]orward
    ["<C-b>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),

    -- Accept ([y]es) the completion.
    --  This will auto-import if your LSP supports it.
    --  This will expand snippets if the LSP sent a snippet.
    ["<C-y>"] = cmp.mapping.confirm({ select = true }),

    -- If you prefer more traditional completion keymaps,
    -- you can uncomment the following lines
    --['<CR>'] = cmp.mapping.confirm { select = true },
    --['<Tab>'] = cmp.mapping.select_next_item(),
    --['<S-Tab>'] = cmp.mapping.select_prev_item(),

    -- Manually trigger a completion from nvim-cmp.
    --  Generally you don't need this, because nvim-cmp will display
    --  completions whenever it has completion options available.
    ["<C-Space>"] = cmp.mapping.complete({}),

    -- Think of <c-l> as moving to the right of your snippet expansion.
    --  So if you have a snippet that's like:
    --  function $name($args)
    --    $body
    --  end
    --
    -- <c-l> will move you to the right of each of the expansion locations.
    -- <c-h> is similar, except moving you backwards.
    ["<C-l>"] = cmp.mapping(function()
      if luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      end
    end, { "i", "s" }),
    ["<C-h>"] = cmp.mapping(function()
      if luasnip.locally_jumpable(-1) then
        luasnip.jump(-1)
      end
    end, { "i", "s" }),

    -- For more advanced Luasnip keymaps (e.g. selecting choice nodes, expansion) see:
    --    https://github.com/L3MON4D3/LuaSnip?tab=readme-ov-file#keymaps
  }),
})

---
-- LSP
---

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("kickstart-lsp-attach", { clear = true }),
  callback = function(event)
    -- NOTE: Remember that Lua is a real programming language, and as such it is possible
    -- to define small helper and utility functions so you don't have to repeat yourself.
    --
    -- In this case, we create a function that lets us more easily define mappings specific
    -- for LSP related items. It sets the mode, buffer and description for us each time.
    local map = function(keys, func, desc, mode)
      mode = mode or "n"
      vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
    end

    -- Jump to the definition of the word under your cursor.
    --  This is where a variable was first declared, or where a function is defined, etc.
    --  To jump back, press <C-t>.
    map("gd", require("telescope.builtin").lsp_definitions, "[G]oto [D]efinition")

    -- Find references for the word under your cursor.
    map("gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")

    -- Jump to the implementation of the word under your cursor.
    --  Useful when your language has ways of declaring types without an actual implementation.
    map("gI", require("telescope.builtin").lsp_implementations, "[G]oto [I]mplementation")

    -- Jump to the type of the word under your cursor.
    --  Useful when you're not sure what type a variable is and you want to see
    --  the definition of its *type*, not where it was *defined*.
    map("<leader>D", require("telescope.builtin").lsp_type_definitions, "Type [D]efinition")

    -- Fuzzy find all the symbols in your current document.
    --  Symbols are things like variables, functions, types, etc.
    map("<leader>ds", require("telescope.builtin").lsp_document_symbols, "[D]ocument [S]ymbols")

    -- Fuzzy find all the symbols in your current workspace.
    --  Similar to document symbols, except searches over your entire project.
    map("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")

    -- Rename the variable under your cursor.
    --  Most Language Servers support renaming across files, etc.
    map("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")

    -- Execute a code action, usually your cursor needs to be on top of an error
    -- or a suggestion from your LSP for this to activate.
    map("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction", { "n", "x" })

    -- WARN: This is not Goto Definition, this is Goto Declaration.
    --  For example, in C this would take you to the header.
    map("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")

    -- The following two autocommands are used to highlight references of the
    -- word under your cursor when your cursor rests there for a little while.
    --    See `:help CursorHold` for information about when this is executed
    --
    -- When you move your cursor, the highlights will be cleared (the second autocommand).
    local client = vim.lsp.get_client_by_id(event.data.client_id)
    -- vim.lsp.protocol
    if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight) then
      local highlight_augroup = vim.api.nvim_create_augroup("kickstart-lsp-highlight", { clear = false })
      vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
        buffer = event.buf,
        group = highlight_augroup,
        callback = vim.lsp.buf.document_highlight,
      })

      vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
        buffer = event.buf,
        group = highlight_augroup,
        callback = vim.lsp.buf.clear_references,
      })

      vim.api.nvim_create_autocmd("LspDetach", {
        group = vim.api.nvim_create_augroup("kickstart-lsp-detach", { clear = true }),
        callback = function(event2)
          vim.lsp.buf.clear_references()
          vim.api.nvim_clear_autocmds({
            group = "kickstart-lsp-highlight",
            buffer = event2.buf,
          })
        end,
      })
    end

    -- The following code creates a keymap to toggle inlay hints in your
    -- code, if the language server you are using supports them
    --
    -- This may be unwanted, since they displace some of your code
    if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
      map("<leader>th", function()
        vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }))
      end, "[T]oggle Inlay [H]ints")
    end
  end,
})

local before_init = function(params)
  params.workDoneToken = "1"
end
local on_attach = function(client, bufnr)
  if IS_CONJURE_LOG(bufnr) then
    vim.lsp.buf_detach_client(bufnr, client.id)
    vim.diagnostic.enable(bufnr, nil)
  end

  vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", { noremap = true })
  vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", { noremap = true })
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<localleader>ld",
    "<cmd>lua vim.lsp.buf.declaration()<CR>",
    { noremap = true, desc = "[L]SP [D]eclartion" }
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<localleader>lt",
    "<cmd>lua vim.lsp.buf.type_definition()<CR>",
    { noremap = true, desc = "[L]SP [T]ype definition" }
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<localleader>lh",
    "<cmd>lua vim.lsp.buf.signature_help()<CR>",
    { noremap = true, "[L] Signature [H]elp" }
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<localleader>ln",
    "<cmd>lua vim.lsp.buf.rename()<CR>",
    { noremap = true, desc = "[L]SP [R]ename" }
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<localleader>le",
    "<cmd>lua vim.diagnostic.open_float()<CR>",
    { noremap = true, desc = "[L] Op[E]n diagnostic" }
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<localleader>lq",
    "<cmd>lua vim.diagnostic.setloclist()<CR>",
    { noremap = true, desc = "[L]ist Diagnostics [Q]" }
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<localleader>lf",
    "<cmd>Format<CR>",
    { noremap = true, desc = "[L]SP [F]ormat buffer" }
  )
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>la", "<cmd>lua vim.lsp.buf.code_action()<CR>", {
    desc = "[L]ist Code [A]actions",
  })
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "v",
    "<localleader>la",
    "<cmd>lua vim.lsp.buf.range_code_action()<CR> ",
    { desc = "[L]ist Code [A]ctions" }
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<localleader>lw",
    "<cmd>lua require('telescope.builtin').diagnostics()<cr>",
    { noremap = true }
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<localleader>cr",
    "<cmd>lua require('telescope.builtin').lsp_references()<cr>",
    { noremap = true }
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<localleader>ci",
    "<cmd>lua require('telescope.builtin').lsp_implementations()<cr>",
    { noremap = true }
  )

  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<localleader>lo",
    '<cmd>lua vim.lsp.buf.code_action({ filter = function(code_action) return string.find(code_action.title, "Clean namespace") end, apply = true, })<cr>',
    { noremap = true }
  )
end

local handlers = {
  ["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics,
    { severity_sort = true, update_in_insert = true, underline = true, virtual_text = false }
  ),
  ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "single" }),
  ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { boder = "single" }),
}

-- LSP servers and clients are able to communicate to each other what features they support.
--  By default, Neovim doesn't support everything that is in the LSP specification.
--  When you add nvim-cmp, luasnip, etc. Neovim now has *more* capabilities.
--  So, we create new capabilities with nvim cmp, and then broadcast that to the servers.
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

-- Enable the following language servers
--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
--
--  Add any additional override configuration in the following tables. Available keys are:
--  - cmd (table): Override the default command used to start the server
--  - filetypes (table): Override the default list of associated filetypes for the server
--  - capabilities (table): Override fields in capabilities. Can be used to disable certain LSP features.
--  - settings (table): Override the default settings passed when initializing the server.
--        For example, to see the options for `lua_ls`, you could go to: https://luals.github.io/wiki/settings/

local lsp = require("lspconfig")
lsp.lua_ls.setup({
  on_attach = on_attach,
  handlers = handlers,
  capabilities = vim.tbl_deep_extend("force", {}, capabilities, { "stylua" }),
  before_init = before_init,
  settings = {
    Lua = {
      completion = {
        callSnippet = "Replace",
      },
      -- You can toggle below to ignore Lua_LS's noisy `missing-fields` warnings
      -- diagnostics = { disable = { 'missing-fields' } },
    },
  },
})

lsp.clojure_lsp.setup({
  on_attach = on_attach,
  handlers = handlers,
  capabilities = capabilities,
  before_init = before_init,
  root_dir = function(pattern)
    local util = require("lspconfig.util")
    local fallback = vim.loop.cwd()
    local patterns = { "project.clj", "deps.edn", "build.boot", "shadow-cljs.edn", ".git", "bb.edn" }
    local root = util.root_pattern(patterns)(pattern)
    if root then
      return root
    else
      return fallback
    end
  end,
})

lsp.nixd.setup({
  on_attach = on_attach,
  handlers = handlers,
  capabilities = capabilities,
  before_init = before_init,
})
