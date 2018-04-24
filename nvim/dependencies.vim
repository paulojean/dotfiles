call plug#begin('~/.local/share/nvim/plugged')

Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-commentary'
Plug 'sheerun/vim-polyglot'
Plug 'kien/rainbow_parentheses.vim'

" Repls
Plug 'BurningEther/iron.nvim'

" Filesystem tinkering
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'm00qek/nvim-contabs'

Plug 'mbbill/undotree'
Plug 'wesQ3/vim-windowswap'
Plug 'myusuf3/numbers.vim'

" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Git stuff
Plug 'mhinz/vim-signify'
Plug 'jreybert/vimagit', { 'branch': 'next' }
Plug 'lambdalisue/gina.vim'
Plug 'rhysd/committia.vim'

" Eyecandy
Plug 'ryanoasis/vim-devicons'

" Code Completion
Plug 'roxma/nvim-completion-manager'

" Clojure
Plug 'guns/vim-sexp', { 'for': 'clojure' }
Plug 'clojure-vim/acid.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'fholiveira/vim-clojure-static', { 'for': 'clojure', 'branch': 'hack-update'}
Plug 'clojure-vim/async-clj-omni'
Plug 'fuadsaud/vim-midje', { 'branch': 'fix-contains' }
Plug 'paulojean/sort-quire.vim'

" Scala
Plug 'ensime/ensime-vim', { 'do': ':UpdateRemotePlugins' }
Plug 'derekwyatt/vim-scala'

" Elm
Plug 'ElmCast/elm-vim'
Plug 'avh4/elm-format'

Plug 'w0rp/ale'

Plug 'Shougo/neco-vim', { 'for': 'vim' }

Plug 'junegunn/vim-easy-align'

Plug 'mileszs/ack.vim'
Plug 'ntpeters/vim-better-whitespace'

Plug 'tommcdo/vim-exchange'
Plug 'machakann/vim-highlightedyank'
Plug 'pseewald/vim-anyfold'

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

" Initialize plugin system
call plug#end()
