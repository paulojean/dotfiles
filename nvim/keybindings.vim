" Leader as ','
let mapleader = ","
let maplocalleader = " "

" rg + fzf ftw
nmap <Leader>/ <plug>Grep
nmap <Leader>? <plug>GrepAll

" magit
"nnoremap <leader>m :MagitOnly<CR>

nnoremap <space>f :Files<CR>
nnoremap <esc>[ :noh<return><esc>

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" undotree
nnoremap ut :UndotreeToggle<cr>
"
" ack.vim
cnoreabbrev Ack Ack!
nnoremap <Leader>a :Ack!<Space>
