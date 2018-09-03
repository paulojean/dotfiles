set t_Co=256
set t_AB=^[[48;5;%dm
set t_AF=^[[38;5;%dm
set background=dark
set termguicolors

" source $HOME/.config/nvim/color.vim

" Better nav
set number
set cursorline
set relativenumber
set hidden

" Indent
set autoindent
set smartindent
set textwidth=100
au BufReadPost,BufNewFile *.md,*.txt,*.tex set tw=999999999
set colorcolumn=+1

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
set fillchars=vert:\│,fold:\─

let g:terminal_scrollback_buffer_size=9999999

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
" let g:loaded_netrwPlugin = 1

let g:is_bash = 1

" highligh terminal cursor when on normal mode
hi! link TermCursor Cursor
hi TermCursorNC ctermfg=235 ctermbg=242 guifg=#002b36 guibg=#586e75 guisp=NONE cterm=NONE gui=NONE

" scala
autocmd BufWritePost *.scala silent :EnTypeCheck

"Airline
let g:airline_powerline_fonts = 0
let g:webdevicons_enable_airline_statusline = 0
let g:airline#extensions#tabline#show_close_button = 0
let g:airline#extensions#tabline#show_splits = 0
let g:airline#extensions#tabline#tab_min_count = 2
let g:airline#extensions#tabline#show_tab_type = 0
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ 'haskell': ['hie', '--lsp'],
    \ 'go': ['go-langserver'],
    \ }

let g:LanguageClient_autoStart = 1
au FileType haskell,rust source ~/.config/nvim/lsp.vim

" nvim-contabs
let g:contabs#project#locations = [
  \ { 'path': '~/nu', 'depth': 1, 'git_only': 1 },
  \ { 'path': '~/.config', 'depth': 1, 'git_only': 0 },
  \ { 'path': '~/proj', 'depth': 1, 'git_only': 1 },
  \ { 'path': '$GOPATH/src', 'depth': 1, 'git_only': 1 }
  \]
command! -nargs=1 -complete=dir EP call contabs#project#edit(<q-args>)
command! -nargs=1 -complete=dir TP call contabs#project#tabedit(<q-args>)

