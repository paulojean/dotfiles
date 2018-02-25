" deoplete tweaking
source $HOME/.config/nvim/plugins/deoplete.vim

" acid tweaking
source $HOME/.config/nvim/plugins/acid.vim


source $HOME/.config/nvim/plugins/vimagit.vim

syntax on
filetype plugin on
filetype plugin indent on
"let anyfold_activate=1
set foldlevel=0

" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'

" Dev Icons
let g:webdevicons_enable = 1
let g:webdevicons_enable_nerdtree = 1
let g:webdevicons_enable_airline_tabline = 1
let g:webdevicons_enable_airline_statusline = 1

"let g:gutentags_ctags_tagfile = ".tags"

let g:signify_vcs_list = [ 'git' ]

let g:python3_host_prog = '/usr/bin/python3'

let g:indentLine_char = '│'
let g:indentLine_first_char = '│'
let g:indentLine_showFirstIndentLevel = 1
let g:indentLine_setColors = 0

" vim-easy-align
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" undotree
if has("persistent_undo")
    set undodir=~/.undodir/
    set undofile
endif

" ack.vim
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" nvim-completion-manager
inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

"fzf.vim
command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>, fzf#vim#with_preview('right:50%'), <bang>0)

let g:airline#extensions#ale#enabled = 1
let g:ale_lint_on_text_changed="never"
let g:ale_lint_on_insert_leave=1

let g:elm_setup_keybindings = 0
let g:elm_format_autosave = 1
