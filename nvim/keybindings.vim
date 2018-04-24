" Leader as ','
let mapleader = ","
let maplocalleader = " "

" magit
"nnoremap <leader>m :MagitOnly<CR>

nnoremap <space><space> :Files<CR>
nnoremap <c-[> :noh<return><esc>
nnoremap <Leader>te :terminal<cr><S-a>
nnoremap <Leader>v :vsp<cr>
nnoremap <Leader>b :sp<cr>
nnoremap <Leader>gp :!git pull<cr>
nnoremap <c-q> :q<cr>
nnoremap <c-s> :w<cr>
inoremap <c-s> <esc>:w<cr>
nnoremap <bs> <c-^>
tnoremap <c-]> <c-\><c-n>

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
noremap <silent><C-F> :Ag<CR>
nnoremap <Leader>a :Ag<space>
nnoremap <Leader>z :Ack --literal "<c-R><c-W>"<CR>

nnoremap <F3> :NumbersToggle<CR>

nnoremap <Leader>sp :source %<CR>

nnoremap <Leader>sq :SortQuire<CR>

nnoremap <Leader>tt :tab sp<cr>:call contabs#project#select()<cr>
nnoremap <silent> <c-p> :call contabs#project#select()<CR>
nnoremap <silent> <c-b> :call contabs#buffer#select()<CR>
