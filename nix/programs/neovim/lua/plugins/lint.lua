return {
	{
		"mfussenegger/nvim-lint",
		event = "LazyFile",
		opts = {
			-- Event to trigger linters
			events = { "BufWritePost", "BufReadPost", "InsertLeave" },

			linters_by_ft = {
				clojure = { "clj-kondo" },
			},
		},
		-- config = function()
		-- 	local lint = require("lint")
		--
		-- 	vim.keymap.set("n", "<leader>lL", function()
		-- 		lint.try_lint()
		-- 	end, { desc = "Trigger linting for current file" })
		-- end,
	},
	{
		"stevearc/conform.nvim",
		optional = true,
		opts = {
			formatters_by_ft = {
				clojure = { "cljfmt" },
			},
		},
		init = function()
			vim.api.nvim_create_user_command("FormatDisable", function(args)
				if args.bang then
					-- FormatDisable! will disable formatting just for this buffer
					vim.b.disable_autoformat = true
				else
					vim.g.disable_autoformat = true
				end
			end, {
				desc = "Disable autoformat-on-save",
				bang = true,
			})

			vim.api.nvim_create_user_command("FormatEnable", function()
				vim.b.disable_autoformat = false
				vim.g.disable_autoformat = false
			end, {
				desc = "Re-enable autoformat-on-save",
			})
		end,
		-- config = function()
		-- 	vim.api.nvim_create_user_command("FormatDisable", function(args)
		-- 		if args.bang then
		-- 			-- FormatDisable! will disable formatting just for this buffer
		-- 			vim.b.disable_autoformat = true
		-- 		else
		-- 			vim.g.disable_autoformat = true
		-- 		end
		-- 	end, {
		-- 		desc = "Disable autoformat-on-save",
		-- 		bang = true,
		-- 	})
		--
		-- 	vim.api.nvim_create_user_command("FormatEnable", function()
		-- 		vim.b.disable_autoformat = false
		-- 		vim.g.disable_autoformat = false
		-- 	end, {
		-- 		desc = "Re-enable autoformat-on-save",
		-- 	})
		--
		-- 	-- require("conform").setup({
		-- 	-- 	format_on_save = function(bufnr)
		-- 	-- 		if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
		-- 	-- 			return
		-- 	-- 		end
		-- 	-- 		return { timeout_ms = 3000, lsp_fallback = true }
		-- 	-- 	end,
		-- 	-- 	formatters_by_ft = {
		-- 	-- 		bash = { "shfmt" },
		-- 	-- 		sh = { "shfmt" },
		-- 	-- 		clojure = { "cljfmt" },
		-- 	-- 	},
		-- 	-- })
		-- end,
	},
}
