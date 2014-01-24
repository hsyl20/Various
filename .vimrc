" Set basic options
"set list " Show tabs and end of lines
set mouse=a
set expandtab " Tabulations converties en espaces
set spelllang=en
set laststatus=2
set nobackup
set ruler " Affiche le numéro de ligne/colonne
set nocompatible " N'essaie pas d'imiter Vi
"set showmatch " Affiche le bracket correspondant à celui sur lequel est le curseur
set incsearch " Recherche incrémentale
set ignorecase " Ignore la casse dans les patterns
set smartcase " N'ignore pas la casse si le pattern contient une majuscule
set hlsearch " Highlight les patterns trouvés lors des recherches

" Set indentation options
set autoindent
set shiftwidth=3 " Indentation automatique
set softtabstop=3 " Indentation manuelle lors de l'edition
set tabstop=3 " Indentation lors de la lecture 

set wildmenu " La barre de completion 
set number " Les numeros de ligne

set encoding=utf-8

let loaded_matchparen = 1

" Autorise le passage d'une ligne a l'autre avec les fleches gauche et droite
" set whichwrap=<,>,[,]

" Activate mouse on console mode
"set mouse=a

" autorise le menu popup avec la souris 
set mousemodel=popup

" met la completion en menu 
set wildmenu
set wildmode=list:longest,full

filetype plugin on

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse latex-suite. Set your grep
" program to alway generate a file-name.
set grepprg=grep\ -nH\ $*


colorscheme desert

" Activate syntax highlighting, filetype plugin and indentation
syntax on
" filetype plugin indent on

" Configure folding
set foldmethod=syntax
set foldenable!

" Specific GUI options
set guifont=Bitstream\ Vera\ Sans\ Mono\ 9

" Set include path for C development
set path+=..
set path+=./include
set path+=../include
set path+=/usr/local/include
set path+=/usr/include/glib-2.0
set path+=/usr/include/gtk-2.0
set path+=/usr/include/libglade-2.0
set path+=/usr/include/ewl
set path+=/usr/include/mpi
set path+=/usr/local/include/graphlib

" Configure tag system
set tags+=$MYUSR/.tags/c

" Prefer tex instead of plaintex
let tex_flavor = 'latex'

" Activate specific bash syntax highlighting and folding
let is_bash = 1
let sh_fold_enabled = 1

" Activate perl folding
let perl_fold = 1
let perl_fold_block = 1

" Configure TagList plugin
let Tlist_Exit_OnlyWindow = 1
let tlist_c_settings = 'c;d:macro;g:enum;s:struct;u:union;t:typedef;v:variable;f:function;p:prototype'
let tlist_cpp_settings = 'c++;n:namespace;v:variable;d:macro;t:typedef;c:class;g:enum;s:struct;u:union;f:function;p:prototype'
nnoremap <silent> <F2> :TlistToggle<CR>
nmap <C-W>e :ConqueTerm zsh<CR>
nmap <C-W>E :ConqueTermVSplit zsh<CR>

"Debut de Completion
function InsertTabWrapper(direction)
  let col = col('.') - 1
  if !col || getline('.')[col - 1] !~ '\k'
    return "\<tab>"
  elseif "backward" == a:direction
    return "\<c-p>"
  else
    return "\<c-n>"
  endif
endfunction
"inoremap <tab> <c-r>=InsertTabWrapper("forward")<cr>
"inoremap <s-tab> <c-r>=InsertTabWrapper("backward")<cr>
" Fin de Completion

runtime ftplugin/man.vim

set guioptions=aegimt

" Add text filetype detection
au BufRead,BufNew *.txt setf text
au BufRead,BufNew README,TODO,BUGS setf text

" Add POSIX Programmer manual detection
au BufRead,BufNew *.3p setf nroff

" Set a convenient text width for some filetypes
au Filetype tex,latex setlocal textwidth=80
au Filetype text setlocal textwidth=80
au Filetype changelog setlocal textwidth=80
au Filetype nroff setlocal textwidth=80

au BufRead,BufNewFile wscript            setfiletype python
au BufRead,BufNewFile *.cl               setfiletype c
au BufRead,BufNewFile *.c.inc               setfiletype c
autocmd FileType man setlocal ro nonumber nolist fdm=indent fdn=2 sw=4 foldlevel=2 | nmap q :quit<CR> | vmap q :quit<CR>

autocmd BufRead,BufNewFile *.c           set tabstop=3 |
                                       \  set expandtab |
                                       \  set shiftwidth=3


autocmd BufRead,BufNewFile *.cl       set ft=c

autocmd BufRead,BufNewFile *.c.inc        set ft=c      |
                                       \  set tabstop=8 |
                                       \  set noexpandtab |
                                       \  set shiftwidth=8

autocmd BufRead,BufNewFile *.hs           set tabstop=8 |
                                       \  set expandtab |
                                       \  set shiftwidth=8

autocmd BufRead,BufNewFile Makefile      set noexpandtab
autocmd BufRead,BufNewFile COMMIT_EDITMSG set spell

autocmd BufRead,BufNewFile *.html       set shiftwidth=4 |
                                      \ set tabstop=4

"Enhanced status bar
set statusline=%F%m%r%h%w\ [%v,%l][%p%%] 
set laststatus=2 

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

"#####################################
"# Haskell
"#####################################
autocmd BufRead *.hsc set ft=haskell

imap <M-w> <C-o>w
imap <M-b> <C-o>b
imap <M-S-w> <C-o><S-w>
imap <M-S-b> <C-o><S-b>
imap <M-S-l> <C-o><S-l>
imap <M-S-h> <C-o><S-h>

imap <M-S-j> <C-o><S-j>
imap <M-S-k> <C-o><S-k>

imap <M-x> <C-o>x
imap <M-S-x> <C-o><S-x>

vmap // y/<C-R>"<CR>
"imap ,, <Esc>
imap ² <Esc>
nmap K :Man -S3 <cword><cr>
imap <C-k> <Esc>vb"ky<Esc>:Man -S3 <cword><cr>j/<C-R>k(<CR>f(y%<Esc>:q<cr>ep!/%a
nmap <tab> :bn<cr>
nmap <s-tab> :bp<cr>
nmap !/ :nohlsearch<cr>

