" deoplete tweaking
source $HOME/.config/nvim/plugins/deoplete.vim

" iron, acid and nvimux tweaking
source $HOME/.config/nvim/plugins/iron_nvimux.vim

" acid tweaking
source $HOME/.config/nvim/plugins/acid.vim

" vim-sexp mappings
"source $HOME/.config/nvim/plugins/vim-sexp.vim


" Dev Icons
let g:webdevicons_enable = 1
let g:webdevicons_enable_nerdtree = 1
let g:webdevicons_enable_airline_tabline = 1
let g:webdevicons_enable_airline_statusline = 1

let g:gutentags_ctags_tagfile = ".tags"

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
