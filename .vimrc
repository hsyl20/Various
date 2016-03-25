" Configuration inspired by:
" http://www.stephendiehl.com/posts/vim_2016.html

syntax on
filetype plugin indent on

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
set clipboard=unnamedplus,autoselect

set completeopt=menuone,menu,longest

" Completion in a menu
set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,.git,.cabal-sandbox
set wildmode=longest,list,full   
set wildmenu
set completeopt+=longest

set t_Co=256

set cmdheight=1

execute pathogen#infect()

colorscheme darkblue

" Syntastic configuration
map <Leader>s :SyntasticToggleMode<CR>

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

" GHC-mod
map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>

" Super-tab
let g:SuperTabDefaultCompletionType = '<c-x><c-o>'
let g:SuperTabMappingForward = '²'

if has("gui_running")
   imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
else " no gui
   if has("unix")
      inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
   endif
endif

let g:haskellmode_completion_ghc = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

" nerdtree
map <Leader>n :NERDTreeToggle<CR>

" tabularize
let g:haskell_tabular = 1

vmap a= :Tabularize /=<CR>
vmap a; :Tabularize /::<CR>
vmap a> :Tabularize /-><CR>
vmap a< :Tabularize /<-<CR>
vmap a- :Tabularize /--<CR>

vmap ad a;gva-

" ctrl-p
map <silent> <Leader>t :CtrlP()<CR>
noremap <leader>b<space> :CtrlPBuffer<cr>
let g:ctrlp_custom_ignore = '\v[\/]dist$'

" ===================================
" Custom
set nobackup

"Enhanced status bar
set statusline=%F%m%r%h%w\ [%v,%l][%p%%] 
set laststatus=2 

set hlsearch " Highlight found patterns

" set spelllang=en
" set laststatus=2
" set ruler " Affiche le numéro de ligne/colonne
" "set showmatch " Affiche le bracket correspondant à celui sur lequel est le curseur
" set ignorecase " Ignore la casse dans les patterns
" 
" " Set indentation options
" set tabstop=3 " Indentation lors de la lecture 
" 
" set encoding=utf-8
" 
" let loaded_matchparen = 1
" 
" " Autorise le passage d'une ligne a l'autre avec les fleches gauche et droite
" " set whichwrap=<,>,[,]
" 
" filetype plugin on
" 
" 
" " Configure folding
" set foldmethod=syntax
" set foldenable!
" 
" " Specific GUI options
" set guifont=Bitstream\ Vera\ Sans\ Mono\ 9
" 
" " Set include path for C development
" set path+=..
" set path+=./include
" set path+=../include
" set path+=/usr/local/include
" 
" " Configure tag system
" set tags+=$MYUSR/.tags/c
" 
" " Prefer tex instead of plaintex
" let tex_flavor = 'latex'
" 
" " Activate specific bash syntax highlighting and folding
" let is_bash = 1
" let sh_fold_enabled = 1
" 
" " Configure TagList plugin
" let Tlist_Exit_OnlyWindow = 1
" let tlist_c_settings = 'c;d:macro;g:enum;s:struct;u:union;t:typedef;v:variable;f:function;p:prototype'
" let tlist_cpp_settings = 'c++;n:namespace;v:variable;d:macro;t:typedef;c:class;g:enum;s:struct;u:union;f:function;p:prototype'
" nnoremap <silent> <F2> :TlistToggle<CR>
" nmap <C-W>e :ConqueTerm zsh<CR>
" nmap <C-W>E :ConqueTermVSplit zsh<CR>
" 
" "Debut de Completion
" function InsertTabWrapper(direction)
"   let col = col('.') - 1
"   if !col || getline('.')[col - 1] !~ '\k'
"     return "\<tab>"
"   elseif "backward" == a:direction
"     return "\<c-p>"
"   else
"     return "\<c-n>"
"   endif
" endfunction
" "inoremap <tab> <c-r>=InsertTabWrapper("forward")<cr>
" "inoremap <s-tab> <c-r>=InsertTabWrapper("backward")<cr>
" " Fin de Completion
" 
" runtime ftplugin/man.vim
" 
" set guioptions=aegimt


" Add text filetype detection
au BufRead,BufNew *.txt setf text
au BufRead,BufNew README,TODO,BUGS setf text

" Add POSIX Programmer manual detection
au BufRead,BufNew *.3p setf nroff

" Set a convenient text width for some filetypes
au Filetype tex,latex setlocal textwidth=80
au Filetype text      setlocal textwidth=80
au Filetype changelog setlocal textwidth=80
au Filetype nroff     setlocal textwidth=80

au BufRead,BufNewFile wscript setfiletype python
au BufRead,BufNewFile *.cl    setfiletype c
au BufRead,BufNewFile *.c.inc setfiletype c

autocmd FileType man setlocal ro nonumber nolist fdm=indent fdn=2 sw=4 foldlevel=2 | nmap q :quit<CR> | vmap q :quit<CR>

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

"#####################################
"# Markdown
"#####################################
autocmd BufRead *.md set ft=markdown

vmap // y/<C-R>"<CR>
"imap ,, <Esc>
nmap K :Man -S3 <cword><cr>
imap <C-k> <Esc>vb"ky<Esc>:Man -S3 <cword><cr>j/<C-R>k(<CR>f(y%<Esc>:q<cr>ep!/%a
nmap <tab> :bn<cr>
nmap <s-tab> :bp<cr>
nmap !/ :nohlsearch<cr>

