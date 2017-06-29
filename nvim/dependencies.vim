call plug#begin('~/.local/share/nvim/plugged')

Plug 'junegunn/vim-easy-align'
Plug 'https://github.com/junegunn/vim-github-dashboard.git'
Plug 'https://github.com/tpope/vim-sexp-mappings-for-regular-people.git'
Plug 'git://github.com/tpope/vim-repeat.git'
Plug 'git://github.com/tpope/vim-classpath.git'
Plug 'git://github.com/tpope/vim-surround.git'
Plug 'git://github.com/tpope/vim-dispatch.git'
Plug 'git://github.com/tpope/vim-projectionist.git'
Plug 'git://github.com/tpope/vim-salve.git'
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
Plug 'hkupty/async-clj-highlight',  { 'for': 'clojure', 'branch': 'acid-autocmd' }

" Vim - Vader
Plug 'Shougo/neco-vim', { 'for': 'vim' }
Plug 'junegunn/vader.vim'

"Ledger
Plug 'ledger/vim-ledger'

Plug 'junegunn/vim-easy-align'

Plug 'git://github.com/begriffs/haskell-vim-now.git'

"Plug 'chr4/sslsecure.vim'

Plug 'mhinz/vim-grepper'

" Initialize plugin system
call plug#end()
