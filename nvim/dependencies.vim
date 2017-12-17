call plug#begin('~/.local/share/nvim/plugged')

" Dark colors
Plug 'nanotech/jellybeans.vim'
Plug 'sjl/badwolf'
Plug 'joshdick/onedark.vim'
Plug 'ayu-theme/ayu-vim', { 'frozen': 1 }

Plug 'junegunn/vim-easy-align'
Plug 'https://github.com/tpope/vim-sexp-mappings-for-regular-people.git'
Plug 'git://github.com/tpope/vim-repeat.git'
Plug 'git://github.com/tpope/vim-classpath.git'
Plug 'git://github.com/tpope/vim-surround.git'
Plug 'git://github.com/tpope/vim-salve.git'
Plug 'git://github.com/tpope/vim-eunuch.git'
Plug 'git://github.com/sheerun/vim-polyglot.git'
Plug 'git://github.com/kien/rainbow_parentheses.vim.git'

" Code Completion
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" Repls
Plug 'hkupty/iron.nvim'

" Filesystem tinkering
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-dirvish'

Plug 'mbbill/undotree'

Plug 'wesQ3/vim-windowswap'
Plug 'qpkorr/vim-bufkill'

" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Git stuff
Plug 'mhinz/vim-signify'
Plug 'jreybert/vimagit', { 'branch': 'next' }
Plug 'lambdalisue/gina.vim'

" Eyecandy
Plug 'ryanoasis/vim-devicons'

" Clojure
Plug 'guns/vim-sexp', { 'for': 'clojure' }
Plug 'clojure-vim/acid.nvim'
Plug 'fholiveira/vim-clojure-static', { 'for': 'clojure', 'branch': 'hack-update'}
Plug 'clojure-vim/async-clj-highlight',  { 'for': 'clojure' }
Plug 'clojure-vim/async-clj-omni'
Plug 'gregspurrier/vim-midje'
Plug 'snoe/clj-refactor.nvim'

Plug 'Shougo/neco-vim', { 'for': 'vim' }

"Ledger
Plug 'ledger/vim-ledger'

Plug 'junegunn/vim-easy-align'

Plug 'mileszs/ack.vim'
Plug 'ntpeters/vim-better-whitespace'


" Initialize plugin system
call plug#end()
