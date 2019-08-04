set t_Co=256
set t_AB=[48;5;%dm
set t_AF=[38;5;%dm
if (has("termguicolors"))
  set termguicolors
endif
set background=dark
colorscheme nord

" Don't overwrite when pasting in visual-mode
xnoremap p "_dP

" Cancel the default behavior of d, D, c, C
"  to put the text they delete in the default register.
nnoremap d "_d
vnoremap d "_d
nnoremap D "_D
vnoremap D "_D
nnoremap c "_c
vnoremap c "_c
nnoremap C "_C
vnoremap C "_C

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

set clipboard+=unnamedplus

" Make whitespaces visible
set list listchars=tab:▷⋅,trail:⋅,nbsp:⋅
set fillchars=vert:\|,fold:\─

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
" hi! link TermCursor Cursor
" hi TermCursorNC ctermfg=235 ctermbg=242 guifg=#002b36 guibg=#586e75 guisp=NONE cterm=NONE gui=NONE

" Lightline
set noshowmode " Hide mode because Lightline handles it
let g:lightline = {
      \ 'colorscheme': 'nord',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ],
      \   'right': [ [ 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'filetype' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'fugitive#head'
      \ },
      \ 'separator': {
      \   'left': '',
      \   'right':''
      \ },
      \ 'subseparator': {
      \   'left': '',
      \   'right': ''
      \ }
      \ }

" Integrate lightline-bufferline with lightline
let g:lightline.tabline          = {'left': [['buffers']], 'right': [['close']]}
let g:lightline.component_expand = {'buffers': 'lightline#bufferline#buffers'}
let g:lightline.component_type   = {'buffers': 'tabsel'}

" Lightline settings
let g:limelight_conceal_ctermfg = 008

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ 'haskell': ['hie', '--lsp'],
    \ 'go': ['go-langserver'],
    \ }

let g:LanguageClient_autoStart = 1
au FileType haskell,rust source ~/.config/nvim/lsp.vim

" nvim-contabs
let g:contabs#project#locations = [
  \ { 'path': '~/code', 'depth': 1, 'git_only': 1 },
  \ { 'path': '~/klarna', 'depth': 1, 'git_only': 1 },
  \ { 'path': '~/.config', 'depth': 1, 'git_only': 0 },
  \ { 'path': '~/proj', 'depth': 1, 'git_only': 1 },
  \ { 'path': '$GOPATH/src', 'depth': 1, 'git_only': 1 }
  \]
command! -nargs=1 -complete=dir EP call contabs#project#edit(<q-args>)
command! -nargs=1 -complete=dir TP call contabs#project#tabedit(<q-args>)

command! -bang -nargs=* LinesWithPreview
      \ call fzf#vim#grep(
      \   'ag --column --line-number --no-heading --smart-case . '.fnameescape(expand('%')), 1,
      \   fzf#vim#with_preview('up:99%'),
      \   <bang>0)
