{ pkgs, ... }:
let
  plugins = pkgs.vimPlugins // pkgs.callPackage ./custom-plugins.nix {};
in {
  programs.neovim = {
    enable = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    withRuby = true;
    withPython3 = true;

    plugins =  with plugins; [
      fzf-vim
      undotree
      ncm2
      ack-vim

      ale
      lightline-ale
      lightline-bufferline

      vim-tmux
      tmux-navigator

      rainbow_parentheses

      nvim-contabs
    ];
    extraConfig = ''
      " general configuration
      set t_Co=256
      set t_AB=[48;5;%dm
      set t_AF=[38;5;%dm
      if (has("termguicolors"))
        set termguicolors
      endif
      set background=dark
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
      " Global taboptions
      set tabstop=2
      set shiftwidth=2
      set expandtab
      " make whitespaces visible
      set list listchars=tab:▷⋅,trail:⋅,nbsp:⋅
      set fillchars=vert:\|,fold:\─
      " Disable a lot of stuff
      let g:loaded_2html_plugin = 1
      let g:loaded_gzip = 1
      let g:loaded_tarPlugin = 1
      let g:loaded_zipPlugin = 1
      " Don't overwrite when pasting in visual-mode
      xnoremap p "_dP


      nnoremap <c-[> :noh<return><esc>

      set clipboard+=unnamedplus
      let g:terminal_scrollback_buffer_size=9999999

      " Leader as ','
      let mapleader = ","
      let maplocalleader = " "

      " keybindings
      nnoremap <space><space> :Files<CR>
      nnoremap <c-q> :q<cr>
      nnoremap <c-s> :w<cr>

      nnoremap <space>a :ALEToggle<CR>
      nnoremap <space>n :NumbersToggle<CR>
      nnoremap <Leader>sp :source %<CR>

      " contabs
      nnoremap <Leader>tt :tab sp<cr>:call contabs#project#select()<cr>
      nnoremap <silent> <c-p> :call contabs#project#select()<CR>
      nnoremap <silent> <c-b> :call contabs#buffer#select()<CR>

      " ack.vim
      cnoreabbrev Ack Ack!
      noremap <silent><C-a> :Ag<CR>
      nnoremap <Leader>a :Ag<space>
      nnoremap <Leader>z :Ack --literal "<c-R><c-W>"<CR>

      " lightline-ale
      let g:lightline = {}
      let g:lightline.component_expand = {
      \  'linter_checking': 'lightline#ale#checking',
      \  'linter_infos': 'lightline#ale#infos',
      \  'linter_warnings': 'lightline#ale#warnings',
      \  'linter_errors': 'lightline#ale#errors',
      \  'linter_ok': 'lightline#ale#ok',
      \ }
      let g:lightline.component_type = {
      \     'linter_checking': 'right',
      \     'linter_infos': 'right',
      \     'linter_warnings': 'warning',
      \     'linter_errors': 'error',
      \     'linter_ok': 'right',
      \ }
      let g:lightline.active = { 'right': [[ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ]] }
      let g:lightline.active = {
      \ 'right': [ [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ],
      \            [ 'lineinfo' ],
      \            [ 'percent' ],
      \            [ 'fileformat', 'fileencoding', 'filetype'] ] }

      " Rainbow Parentheses
      au VimEnter * RainbowParenthesesToggle
      au Syntax * RainbowParenthesesLoadRound
      au Syntax * RainbowParenthesesLoadSquare
      au Syntax * RainbowParenthesesLoadBraces

      " Lightline settings
      let g:limelight_conceal_ctermfg = 008

      " Integrate lightline-bufferline with lightline
      let g:lightline.tabline          = {'left': [['buffers']], 'right': [['close']]}
      let g:lightline.component_expand = {'buffers': 'lightline#bufferline#buffers'}
      let g:lightline.component_type   = {'buffers': 'tabsel'}

      " nvim-contabs
      let g:contabs#project#locations = [
      \ { 'path': '~/code', 'depth': 1, 'git_only': 1 },
      \ { 'path': '~/.config', 'depth': 1, 'git_only': 0 }
      \]
      command! -nargs=1 -complete=dir EP call contabs#project#edit(<q-args>)
      command! -nargs=1 -complete=dir TP call contabs#project#tabedit(<q-args>)

      command! -bang -nargs=* LinesWithPreview
      \ call fzf#vim#grep(
      \   'ag --column --line-number --no-heading --smart-case . '.fnameescape(expand('%')), 1,
      \   fzf#vim#with_preview('up:99%'),
      \   <bang>0)
    '';
  };
}
