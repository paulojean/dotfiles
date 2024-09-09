function is_conjure_log(buf)
  local bufname = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":t")
  return string.sub(bufname, 1, 12) == "conjure-log-"
end

return {
  {
    "neovim/nvim-lspconfig",
    event = "LazyFile",
    dependencies = {
      "clojure-lsp/clojure-lsp",
      "nvim-telescope/telescope.nvim",
    },
    opts = function()
      return {
        servers = {
          clojure_lsp = {
            mason = false,
            settings = {
              completion = {}
            }
          },
          nixd = {
            mason = false,
          },
        },
      }
    end,
    config = function(_, opts)
      LazyVim.format.register(LazyVim.lsp.formatter())
      LazyVim.lsp.on_attach(function(client, buffer)
        require("lazyvim.plugins.lsp.keymaps").on_attach(client, buffer)
      end)
      LazyVim.lsp.setup()
      LazyVim.lsp.on_dynamic_capability(require("lazyvim.plugins.lsp.keymaps").on_attach)
      LazyVim.lsp.words.setup(opts.document_highlight)


      local lsp = require("lspconfig")
      local cmplsp = require("cmp_nvim_lsp")
      local handlers = {
        ["textDocument/publishDiagnostics"] = vim.lsp.with(
          vim.lsp.diagnostic.on_publish_diagnostics,
          { severity_sort = true, update_in_insert = true, underline = true, virtual_text = false }

        ),
        ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "single" }),
        ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { boder = "single" }),
      }
      local capabilities = cmplsp.default_capabilities()
      local before_init = function(params)
        params.workDoneToken = "1"
      end
      local on_attach = function(client, bufnr)
        if is_conjure_log(bufnr) then
          vim.lsp.buf_detach_client(bufnr, client.id)
        end

        vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<Cmd>lua vim.lsp.buf.definition()<CR>", { noremap = true })
        vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>", { noremap = true })
        vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>ld", "<cmd>lua vim.lsp.buf.declaration()<CR>",
          { noremap = true })
        vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>lt", "<cmd>lua vim.lsp.buf.type_definition()<CR>",
          { noremap = true })
        vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>lh", "<cmd>lua vim.lsp.buf.signature_help()<CR>",
          { noremap = true })
        vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>ln", "<cmd>lua vim.lsp.buf.rename()<CR>",
          { noremap = true })
        vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>le", "<cmd>lua vim.diagnostic.open_float()<CR>",
          { noremap = true })
        vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>lq", "<cmd>lua vim.diagnostic.setloclist()<CR>",
          { noremap = true })
        vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>lf", "<cmd>lua vim.lsp.buf.format()<CR>",
          { noremap = true })
        -- vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>lj", "<cmd>lua vim.diagnostic.goto_next()<CR>", {
        --   noremap = true })
        -- vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>lk", "<cmd>lua vim.diagnostic.goto_prev()<CR>", {
        --   noremap = true })
        vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>la", "<cmd>lua vim.lsp.buf.code_action()<CR>", {
          noremap =
              true
        })
        vim.api.nvim_buf_set_keymap(bufnr, "v", "<localleader>la", "<cmd>lua vim.lsp.buf.range_code_action()<CR> ",
          { noremap = true })
        vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>lw", ":lua require('telescope.builtin').diagnostics()<cr>",
          { noremap = true })
        vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>lr",
          ":lua require('telescope.builtin').lsp_references()<cr>",
          { noremap = true })
        vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>li",
          ":lua require('telescope.builtin').lsp_implementations()<cr>", { noremap = true })

        vim.api.nvim_buf_set_keymap(bufnr, "n", "<localleader>lo",
          ':lua callback = vim.lsp.buf.code_action({ filter = function(code_action) return string.find(code_action.title, "Clean namespace") end, apply = true, })<cr>',
          { noremap = true })
      end

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
        end
      })

      lsp.lua_ls.setup({
        on_attach = on_attach,
        handlers = handlers,
        capabilities = capabilities,
        before_init = before_init,
      })

      lsp.nixd.setup({
        on_attach = on_attach,
        handlers = handlers,
        capabilities = capabilities,
        before_init = before_init,
      })
    end,
  },
  { "clojure-lsp/clojure-lsp" },
  {
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-path",
  },
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      highlight = {
        enable = true,
        disable = function(lang, buf)
          return is_conjure_log(buf)
        end,
      }
    },
    config = function(_, opts)
      -- -- Set up Treesitter, but not for the log buffer
      -- require("nvim-treesitter.configs").setup {
      --   -- ensure_installed = { "clojure" },
      --   auto_install = true,
      --   highlight = {
      --     enable = true,
      --     disable = function(lang, buf)
      --       return is_conjure_log(buf)
      --     end,
      --   },
      --   incremental_selection = {
      --     enable = true,
      --     disable = function(lang, buf)
      --       return is_conjure_log(buf)
      --     end,
      --   },
      --   textobjects = {
      --     enable = true,
      --     disable = function(lang, buf)
      --       return is_conjure_log(buf)
      --     end,
      --   },
      --   indent = {
      --     enable = true,
      --     disable = function(lang, buf)
      --       return is_conjure_log(buf)
      --     end,
      --   },
      -- }
      --
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
  }
}
