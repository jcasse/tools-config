" vimrc:
" Maintainer: Juan Casse

" Type :so % to refresh .vimrc after making changes

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" git wrapper
Plugin 'tpope/vim-fugitive'

" status bar
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins;
"                     append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins;
"                     append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:airline_powerline_fonts = 1
if !exists('g:airline_symbols')
let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
"let g:airline_symbols.linenr = '¶'
let g:airline_symbols.maxlinenr = '☰'
"let g:airline_symbols.maxlinenr = ''
"let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
"let g:airline_symbols.paste = 'Þ'
"let g:airline_symbols.paste = '∥'
let g:airline_symbols.spell = 'Ꞩ'
let g:airline_symbols.notexists = '∄'
let g:airline_symbols.whitespace = 'Ξ'

" powerline symbols
"let g:airline_left_sep = ''
"let g:airline_left_alt_sep = ''
"let g:airline_right_sep = ''
"let g:airline_right_alt_sep = ''
"let g:airline_symbols.branch = ''
"let g:airline_symbols.readonly = ''
"let g:airline_symbols.linenr = ''

" old vim-powerline symbols
"let g:airline_left_sep = '⮀'
let g:airline_left_alt_sep = '⮁'
"let g:airline_right_sep = '⮂'
let g:airline_right_alt_sep = '⮃'
let g:airline_symbols.branch = '⭠'
let g:airline_symbols.readonly = '⭤'
"let g:airline_symbols.linenr = '⭡'

" color schemes installed in /usr/share/vim/vim73/colors
colo desert
syntax on

" basic configuration
set showmatch                   " shows matching brackets/parenthesis
set hlsearch                    " highlights search results
set scrolloff=3                 " keeps minimum lines above/below cursor
"set title                       " shows window title
set tabstop=4                   " uses 4 spaces for tabs
set shiftwidth=4
set expandtab                   " converts tab to spaces
set fileformat=unix             " uses unix file format by default
set fileformats=unix,dos,mac    " sets available formats
set modelines=5                 " checks lines top/bottom for special comment
set laststatus=2                " display status line always
"set statusline=\
"    \ FILE\ %F%m%r%h%w\
"    \ FORMAT\ %{&ff}\
"    \ TYPE\ %Y\
"    \ LINES\ %L\
"    \ POS\ %l,%v\ %p%%
match ErrorMsg '\%>79v.\+'      " highlight text from column 80 onwards
                                " http://vim.wikia.com/wiki/Highlight_long_lines

set autowrite     " Automatically :write before running commands
set autoread      " Reload files changed outside vim
" Trigger autoread when changing buffers or coming back to vim in terminal.
au FocusGained,BufEnter * :silent! !

set visualbell    " stop that ANNOYING beeping

" do not leave a trace
set viminfo='0,:0,<0,@0,f0,/0,n~/.viminfo

" Display extra whitespace
set list listchars=tab:»·,trail:·,nbsp:·

" Numbers
set number
set numberwidth=5

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Toggle relative numbering, and set to absolute on loss of focus or insert mode
set rnu
function! ToggleNumbersOn()
    set nu!
    set rnu
endfunction
function! ToggleRelativeOn()
    set rnu!
    set nu
endfunction
autocmd FocusLost * call ToggleRelativeOn()
autocmd FocusGained * call ToggleRelativeOn()
autocmd InsertEnter * call ToggleRelativeOn()
autocmd InsertLeave * call ToggleRelativeOn()
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Open new split panes to right and bottom, which feels more natural
" set splitbelow
set splitright

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Auto resize Vim splits to active split
set winwidth=104
set winheight=5
set winminheight=5
set winheight=999
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" applies c++ color scheme to these file extensions
"autocmd BufNewFile,BufReadPost *.inc,*.tpp set filetype=cpp

" turns on spell checker for these file extensions
autocmd BufNewFile,BufReadPost *.tex set spell spelllang=en_us

" keep tabs in makefiles
autocmd FileType make setlocal noexpandtab

" sets pdf viewer for latexliveviewer plugin
let g:livepreview_previewer = 'open -a Preview'
