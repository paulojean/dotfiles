set background=dark

" Better nav
set number
set cursorline
set relativenumber
set hidden

" Indent
set autoindent
set smartindent

" Base sanity stuff
set laststatus=2
set noswapfile
set autowriteall

" Also, let me have settings per project
set exrc
set secure

" Global taboptions
set tabstop=2
set shiftwidth=2
set expandtab

" Option complete
set wildmode=full

" Live substitution
set inccommand=split

" Make whitespaces visible
set list listchars=tab:▷⋅,trail:⋅,nbsp:⋅
"set fillchars=vert:\│,fold:\─<Paste>

" Rainbow Parentheses
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

" Disable a lot of stuff
let g:loaded_2html_plugin = 1
let g:loaded_gzip = 1
let g:loaded_tarPlugin = 1
let g:loaded_zipPlugin = 1
let g:loaded_netrwPlugin = 1
