call plug#begin('~/.local/share/nvim/plugged')

Plug 'christoomey/vim-tmux-navigator'

Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-commentary'
Plug 'sheerun/vim-polyglot'
Plug 'kien/rainbow_parentheses.vim'

Plug 'Vigemus/impromptu.nvim'

" Repls
Plug 'Vigemus/iron.nvim'

" Filesystem tinkering
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'm00qek/nvim-contabs'

Plug 'mbbill/undotree'
Plug 'wesQ3/vim-windowswap'
Plug 'myusuf3/numbers.vim'

" Airline
Plug 'itchyny/lightline.vim' " a really cool status bar
Plug 'mgee/lightline-bufferline' " Show buffers in tabline

Plug 'arcticicestudio/nord-vim', { 'branch': 'develop' }
Plug 'colepeters/spacemacs-theme.vim'

" Git stuff
" Plug 'mhinz/vim-signify'
Plug 'jreybert/vimagit', { 'branch': 'next' }
Plug 'lambdalisue/gina.vim'
Plug 'rhysd/committia.vim'
Plug 'Vigemus/pointer.nvim'

" Eyecandy
Plug 'ryanoasis/vim-devicons'
" Plug 'KKPMW/sacredforest-vim'

" Code Completion
Plug 'ncm2/ncm2'

" Clojure
Plug 'guns/vim-sexp', { 'for': 'clojure' }
Plug 'clojure-vim/acid.nvim', { 'branch': 'admin-nrepl', 'do': ':UpdateRemotePlugins' }
" Plug 'clojure-vim/jazz.nvim', { 'branch': 'deps-edn-options' }
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'fholiveira/vim-clojure-static', { 'for': 'clojure', 'branch': 'hack-update'}
Plug 'clojure-vim/async-clj-omni'
Plug 'fuadsaud/vim-midje', { 'branch': 'fix-contains' }
Plug 'paulojean/sort-quire.vim'

Plug 'w0rp/ale'

Plug 'Shougo/neco-vim', { 'for': 'vim' }

Plug 'junegunn/vim-easy-align'

Plug 'mileszs/ack.vim'
Plug 'ntpeters/vim-better-whitespace'

Plug 'tommcdo/vim-exchange'
Plug 'machakann/vim-highlightedyank'
Plug 'pseewald/vim-anyfold'

" Nix
" Plug 'MarcWeber/vim-addon-nix'

"Plug 'autozimu/LanguageClient-neovim', {
"    \ 'branch': 'next',
"    \ 'do': 'bash install.sh',
"    \ }

" Initialize plugin system
call plug#end()
