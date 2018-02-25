" Leader as ','
let mapleader = ","
let maplocalleader = " "

" magit
"nnoremap <leader>m :MagitOnly<CR>

nnoremap <space>f :Files<CR>
nnoremap <c-[> :noh<return><esc>
nnoremap <Leader>te :terminal<cr><S-a>
nnoremap <Leader>v :vsp<cr>
nnoremap <Leader>b :sp<cr>
nnoremap <c-s> :w<cr>
nnoremap <bs> <c-^>

tnoremap <c-h> <c-\><c-n><c-w>h
tnoremap <c-j> <c-\><c-n><c-w>j
tnoremap <c-k> <c-\><c-n><c-w>k
tnoremap <c-l> <c-\><c-n><c-w>l

inoremap <c-h> <Esc><c-w>h
inoremap <c-j> <Esc><c-w>j
inoremap <c-k> <Esc><c-w>k
inoremap <c-l> <Esc><c-w>l

vnoremap <c-h> <Esc><c-w>h
vnoremap <c-j> <Esc><c-w>j
vnoremap <c-k> <Esc><c-w>k
vnoremap <c-l> <Esc><c-w>l

nnoremap <c-j> <c-w><c-j>
nnoremap <c-k> <c-w><c-k>
nnoremap <c-l> <c-w><c-l>
nnoremap <c-h> <c-w><c-h>

" A-r paste from buffer on terminal in insert mode
tnoremap <expr> <A-r> '<C-\><C-N>"'.nr2char(getchar()).'pi'

" undotree
nnoremap tt :UndotreeToggle<cr>

" ack.vim
cnoreabbrev Ack Ack!
nnoremap <Leader>a :Ag<Space>
nnoremap <leader>z :Ack --literal "<c-R><c-W>"<CR>

nnoremap <F3> :NumbersToggle<CR>

nnoremap <Leader>tt :tab sp<return>:cd<space>
tnoremap <c-]> <c-\><c-n>

nnoremap <Leader>sp :source %<CR>

nnoremap <Leader>sq :SortQuire<CR>
