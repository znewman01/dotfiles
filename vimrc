syn on "Turns on syntax highlighting 
filetype plugin indent on  "Filetype detection

set nocompatible
set encoding=utf-8  "Unicode ALL the files!   
set nu  "Turns on line numbers
set tabstop=4
set shiftwidth=4 "a tab is 4 spaces

set expandtab  "A tab is automatically expanded into spaces
set incsearch "Shows first match for pattern while still typing it
set hlsearch "Highlights matches for pattern
set visualbell " Visual bell instead of audible
set hidden
set backspace=indent,eol,start

set pastetoggle=<F2>

let mapleader=","
nnoremap <leader>n :bnext<CR>
nnoremap <leader>p :bprevious<CR>
nnoremap <leader>w :write<CR>
nnoremap <leader>x :xit<CR>
nnoremap <leader>d :bd<CR>
nnoremap <leader>q :quit<CR>
nnoremap \| :vsplit<CR>
nnoremap _ :split<CR>
nnoremap <leader>sv :source ~/.vimrc<CR>

"CTRL + L will not only clear screen, but also remove the highlighting
"for the last search as well
nnoremap <leader>/ :nohlsearch<CR>

"reflow paragraph and move to end of it
nnoremap Q gqapk$

colorscheme peachpuff " Optional. Type :colorscheme and tab through to try different ones out.

"Disable arrow keys
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>

"set runtimepath^=~/.vim/bundle/ctrlp.vim

"call pathogen#infect()
"call pathogen#helptags()
