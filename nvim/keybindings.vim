" Leader as ','
let mapleader = ","
let maplocalleader = " "

" magit
"nnoremap <leader>m :MagitOnly<CR>

nnoremap <space>f :Files<CR>
nnoremap <C-[> :noh<return><esc>
nnoremap <Leader>tt :tab sp<return>
nnoremap <Leader>te :terminal<return>
nnoremap <Leader>v :vsp<return>
nnoremap <Leader>b :sp<return>

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" undotree
nnoremap tt :UndotreeToggle<cr>

" ack.vim
cnoreabbrev Ack Ack!
nnoremap <Leader>a :Ack!<Space>

tnoremap <Esc> <C-\><C-n>
