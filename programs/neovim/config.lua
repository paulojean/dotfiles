-- general configuration
if vim.fn.has('termguicolors') == 1 then
   vim.opt.termguicolors = true
end
vim.opt.background = 'dark'
vim.cmd('colorscheme gruvbox')

-- Better nav
vim.opt.number = true
vim.opt.cursorline = true
vim.opt.relativenumber = true
vim.opt.hidden = true

-- Indent
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.textwidth = 100
-- vim.opt.colorcolumn = vim.opt.colorcolumn + 1
vim.api.nvim_create_autocmd({"BufReadPost", "BufNewFile"}, {
      pattern = {"*.md", "*.txt", "*.tex"},
      callback = function ()
         vim.opt.tw=999999999
      end
})


-- Base sanity stuff
vim.opt.laststatus = 2
-- vim.opt.noswapfile = true
vim.opt.autowriteall = true
-- Global taboptions
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
-- make whitespaces visible
vim.opt.listchars = 'tab:▷⋅,trail:⋅,nbsp:⋅'
-- Disable a lot of stuff
vim.g.loaded_2html_plugin = 1
vim.g.loaded_gzip = 1
vim.g.loaded_tarPlugin = 1
vim.g.loaded_zipPlugin = 1


vim.api.nvim_set_keymap( 'n',   '<esc>', ':noh<CR><esc>', {noremap = true})

-- vim.opt.clipboard = vim.opt.clipboard + 'unnamedplus'
vim.g.terminal_scrollback_buffer_size = 9999999

-- Leader as ','
vim.g.mapleader = ','
vim.g.maplocalleader = ' '

-- keybindings
vim.api.nvim_set_keymap( 'n',   '<space><space>', ':Files<CR>', {noremap = true})
vim.api.nvim_set_keymap( 'n',   '<C-q>', ':q<CR>', {noremap = true})
vim.api.nvim_set_keymap( 'n',   '<C-s>', ':w<CR>', {noremap = true})

vim.api.nvim_set_keymap( 'n',   '<space>a', ':ALEToggle<CR>', {noremap = true})
vim.api.nvim_set_keymap( 'n',   '<space>n', ':NumbersToggle<CR>', {noremap = true})
vim.api.nvim_set_keymap( 'n',   '<Leader>sp', ':source %<CR>', {noremap = true})


-- ack.vim
vim.cmd([[
   cnoreabbrev Ack Ack!
]])
vim.api.nvim_set_keymap( 'n',   '<C-a>', ':Ag<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap( 'n',   '<Leader>a', ':Ag<space>', {noremap = true})
vim.api.nvim_set_keymap( 'n',   '<Leader>z', ':Ack --literal "<c-R><c-W>"<CR>', {noremap = true})


-- lightline
vim.g.lightline = {
   -- lightline-ale
   component_expand = {
      linter_checking = 'lightline#ale  #checking',
      linter_infos = 'lightline#ale#infos',
      linter_warnings = 'lightline#ale#warnings',
      linter_errors = 'lightline#ale#errors',
      linter_ok = 'lightline#ale#ok',
   },
   component_type = {
      linter_checking = 'right',
      linter_infos = 'right',
      linter_warnings = 'warning',
      linter_errors = 'error',
      linter_ok = 'right',
      buffers = 'tabsel',
   },
   active = {
      right = {
         { 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' },
         { 'lineinfo' },
         { 'percent' },
         { 'fileformat', 'fileencoding', 'filetype' }
      },
   },
   -- Integrate lightline-bufferline with lightline
   tabline = {left = {{'buffers'}}, right = {{'close'}}},
   component_expand = {buffers = 'lightline#bufferline#buffers'}
}


-- contabs
vim.api.nvim_set_keymap( 'n',   '<silent>tt', ':tab sp<CR>:call contabs#project#select()<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap( 'n',   '<C-p>', ':call contabs#project#select()<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap( 'n',   '<C-b>', ':call contabs#buffer#select()<CR>', {noremap = true, silent = true})
vim.g['contabs#project#locations'] = {
   {  path  = '~/code', depth = 1, git_only = 1 },
   {  path = '~/.config', depth = 1, git_only = 0 },
}
vim.api.nvim_create_user_command('EP', 'call contabs#project#edit(<q-args>)', { nargs = 1 })
vim.api.nvim_create_user_command('TP', 'call contabs#project#tabedit(<q-args>)', { nargs = 1 })
