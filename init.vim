call plug#begin('~/.local/share/nvim/plugged')

Plug 'mhinz/vim-startify'
Plug 'morhetz/gruvbox'
Plug 'neomake/neomake'
Plug 'parsonsmatt/intero-neovim'
Plug 'godlygeek/tabular'

call plug#end()

augroup interoMaps
  au!
  " Maps for intero. Restrict to Haskell buffers so the bindings don't collide.

  " Background process and window management
  au FileType haskell nnoremap <silent> <leader>is :InteroStart<CR>
  au FileType haskell nnoremap <silent> <leader>ik :InteroKill<CR>

  " Open intero/GHCi split horizontally
  au FileType haskell nnoremap <silent> <leader>io :InteroOpen<CR>
  " Open intero/GHCi split vertically
  au FileType haskell nnoremap <silent> <leader>iov :InteroOpen<CR><C-W>H
  au FileType haskell nnoremap <silent> <leader>ih :InteroHide<CR>

  " Reloading (pick one)
  " Automatically reload on save
  au BufWritePost *.hs InteroReload
  " Manually save and reload
  au FileType haskell nnoremap <silent> <leader>wr :w \| :InteroReload<CR>

  " Load individual modules
  au FileType haskell nnoremap <silent> <leader>il :InteroLoadCurrentModule<CR>
  au FileType haskell nnoremap <silent> <leader>if :InteroLoadCurrentFile<CR>

  " Type-related information
  " Heads up! These next two differ from the rest.
  au FileType haskell map <silent> <leader>t <Plug>InteroGenericType
  au FileType haskell map <silent> <leader>T <Plug>InteroType
  au FileType haskell nnoremap <silent> <leader>it :InteroTypeInsert<CR>

  " Navigation
  au FileType haskell nnoremap <silent> <leader>jd :InteroGoToDef<CR>

  " Managing targets
  " Prompts you to enter targets (no silent):
  au FileType haskell nnoremap <leader>ist :InteroSetTargets<SPACE>
augroup END

" Intero starts automatically. Set this if you'd like to prevent that.
" let g:intero_start_immediately = 0

" Enable type information on hover (when holding cursor at point for ~1 second).
" let g:intero_type_on_hover = 1

" Change the intero window size; default is 10.
" let g:intero_window_size = 15

" Sets the intero window to split vertically; default is horizontal
" let g:intero_vertical_split = 1

" OPTIONAL: Make the update time shorter, so the type info will trigger faster.
" set updatetime=1000



" tabularize
vmap a= :Tabularize /=<CR>
vmap a; :Tabularize /::<CR>
vmap a> :Tabularize /-><CR>
vmap a< :Tabularize /<-<CR>
vmap a- :Tabularize /--<CR>

vmap ad a;gva-

set nofoldenable     " Disable folding
set nocompatible     " Don't try to imitate vi
set number           " Show line numbers
" set nowrap
set showmode
set tw=80
set smartcase        " Don't ignore case if the pattern contains a CAPITAL
set smarttab
set smartindent
set autoindent       " Auto-indentation
set softtabstop=3    " Use 3 spaces for tab
set shiftwidth=3     " Use 3 spaces for indent
set expandtab        " Convert tabs into spaces
set incsearch        " Incremental search
set mouse=a          " Enable the mouse
set history=1000
set nobackup
set hlsearch " Highlight found patterns

" Completion in a menu
set completeopt=menuone,menu,longest

set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,.git,.cabal-sandbox
set wildmode=longest,list,full   
set wildmenu

set cmdheight=1

colorscheme gruvbox
set background=light

" Disable Background Color Erase (BCE) so that color schemes
" work properly when we scroll
set t_ut=

" Terminal supports 256 colors
set t_Co=256

au BufRead,BufNewFile *.cmm   setfiletype c

autocmd BufRead,BufNewFile *.c           set tabstop=3 |
                                       \  set expandtab |
                                       \  set shiftwidth=3


autocmd BufRead,BufNewFile *.cl       set ft=c

autocmd BufRead,BufNewFile *.c.inc        set ft=c      |
                                       \  set tabstop=8 |
                                       \  set noexpandtab |
                                       \  set shiftwidth=8

autocmd BufRead,BufNewFile Makefile      set noexpandtab |
                                       \ set nosmarttab
autocmd BufRead,BufNewFile COMMIT_EDITMSG set spell

autocmd BufRead,BufNewFile *.html       set shiftwidth=4 |
                                      \ set tabstop=4
autocmd BufRead,BufNewFile *.hsc set ft=haskell

command Trim %s/\s\+$//e

"########################################
"# Sauvegarde position dans les fichiers
"########################################
" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \   exe "normal g`\"" |
      \ endif

