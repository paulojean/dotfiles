" " Indent
set colorcolumn=+1
set noswapfile
" make whitespaces visible
set list listchars=tab:▷⋅,trail:⋅,nbsp:⋅
set fillchars=vert:\|,fold:\─


set clipboard+=unnamedplus


" Rainbow Parentheses
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces


command! -bang -nargs=* LinesWithPreview
\ call fzf#vim#grep(
\   'ag --column --line-number --no-heading --smart-case . '.fnameescape(expand('%')), 1,
\   fzf#vim#with_preview('up:99%'),
\   <bang>0)
