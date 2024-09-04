return {
	{ "Olical/conjure" },
	{
		"PaterJason/cmp-conjure",
		-- config = function()
		--    local cmp = require("cmp")
		--    local config = cmp.get_config()
		--    table.insert(config.sources, { name = "conjure" })
		--    return cmp.setup(config)
		-- end,
	},
	{ "PaterJason/nvim-treesitter-sexp" },
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
