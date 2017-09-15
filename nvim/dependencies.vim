call plug#begin('~/.local/share/nvim/plugged')

" Dark colors
Plug 'nanotech/jellybeans.vim'
Plug 'sjl/badwolf'
Plug 'joshdick/onedark.vim'
Plug 'ayu-theme/ayu-vim', { 'frozen': 1 }

Plug 'junegunn/vim-easy-align'
Plug 'https://github.com/junegunn/vim-github-dashboard.git'
Plug 'https://github.com/tpope/vim-sexp-mappings-for-regular-people.git'
Plug 'git://github.com/tpope/vim-repeat.git'
Plug 'git://github.com/tpope/vim-classpath.git'
Plug 'git://github.com/tpope/vim-surround.git'
Plug 'git://github.com/tpope/vim-dispatch.git'
Plug 'git://github.com/tpope/vim-projectionist.git'
Plug 'git://github.com/tpope/vim-salve.git'
Plug 'git://github.com/tpope/vim-eunuch.git'
Plug 'git://github.com/sheerun/vim-polyglot.git'
Plug 'git://github.com/kien/rainbow_parentheses.vim.git'

" Code Completion
Plug 'shougo/deoplete.nvim'
Plug 'Shougo/denite.nvim'

" Repls
Plug 'hkupty/iron.nvim', { 'branch': 'testing' }

" Filesystem tinkering
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-dirvish'

" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Git stuff
Plug 'mhinz/vim-signify'
Plug 'jreybert/vimagit', { 'branch': 'next' }
Plug 'lambdalisue/gina.vim'

" Code navigation
Plug 'unblevable/quick-scope', { 'on': 'QuickScopeToggle' }

" Align
Plug 'tommcdo/vim-lion'

" Eyecandy
Plug 'ryanoasis/vim-devicons'

" Clojure
Plug 'guns/vim-sexp', { 'for': 'clojure' }
Plug 'clojure-vim/acid.nvim', { 'branch': 'testing' }
Plug 'clojure-vim/async-clj-omni'
Plug 'guns/vim-clojure-static', { 'for': 'clojure' }
Plug 'clojure-vim/async-clj-highlight',  { 'for': 'clojure' }
Plug 'gregspurrier/vim-midje'

" Vim - Vader
Plug 'Shougo/neco-vim', { 'for': 'vim' }
Plug 'junegunn/vader.vim'

"Ledger
Plug 'ledger/vim-ledger'

Plug 'junegunn/vim-easy-align'

Plug 'git://github.com/begriffs/haskell-vim-now.git'

"Plug 'chr4/sslsecure.vim'

Plug 'mhinz/vim-grepper'
Plug 'ntpeters/vim-better-whitespace'


" Java :hue:
Plug 'artur-shaik/vim-javacomplete2'

" Initialize plugin system
call plug#end()
