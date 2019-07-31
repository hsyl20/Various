" Configuration inspired by:
" http://www.stephendiehl.com/posts/vim_2016.html
" https://www.tpflug.me/2019/01/14/haskell-nix-vim/

syntax on
filetype plugin indent on

execute pathogen#infect()

set belloff=all      " disable bells
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
set clipboard=unnamedplus,autoselect

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



" Syntastic configuration
map <Leader>s :SyntasticToggleMode<CR>

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

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
au BufRead,BufNewFile *.cmm   setfiletype c

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

command Trim %s/\s\+$//e
