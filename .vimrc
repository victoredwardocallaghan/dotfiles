"
" Copyright © 2012 Edward O'Callaghan. All Rights Reserved.
"
" File: ~/.vimrc
" Author: Edward O'Callaghan
" Description: 
" Last Modified: June 18, 2012

set nocompatible " be ViMproved


" general
" -------

set t_Co=256 " force 256-color mode"
set nobackup " disable backup files (filename~)
set splitbelow " place new files below the current
set clipboard+=unnamed " yank and copy to X clipboard
set encoding=utf-8 " UTF-8 encoding for all new files
set mouse=a " allow mouse input in all modes
set ttymouse=xterm " enable scrolling in screen (xterm2) or tmux (xterm) sessions
set backspace=2 " full backspacing capabilities (indent,eol,start)
set number " show line numbers"
set ww=b,s,h,l,<,>,[,] " whichwrap -- left/right keys can traverse up/down
set linebreak " attempt to wrap lines cleanly
set wildmenu " enhanced tab-completion shows all matching cmds in a popup menu
set wildmode=list:longest,full"
set cursorcolumn " highlight current column
set cursorline " hightlight current line
set spell " enable spell checking
syntax on
filetype plugin indent on


" hotkeys
" -------

set pastetoggle=<F2> " F2 toggles paste mode"

" strip ^M linebreaks from dos formatted files
map M :%s/
$//g


" theme
" -----
let g:zenburn_high_Contrast = 1
colors zenburn


" active vim-pathogen
call pathogen#infect()
call pathogen#helptags()


" tabs and indenting
" ------------------

set smartindent
" (sw)shiftwidth: how many columns text is indented with reindent operations
" (sts)softtabstop: how many columns vim uses when you hit tab
" (ts)tabstop: how many columns a tab counts for
set ts=4 sw=4 sts=2


" searching
" ---------

set hlsearch " highlight all search results
set incsearch " increment search
set ignorecase " case-insensitive search
set smartcase " uppercase causes case-sensitive search


" statusline (:help statusline for details)
" -----------------------------------------

set laststatus=2 " condition to show status line, 2=always.
set ruler " show cursor position in status line
set showmode " show mode in status line
set showcmd " show partial commands in status line
" left: fileformat, type, encoding, RO/HELP/PREVIEW, modified flag, filepath
set statusline=%([%{&ff}]%)%(:[%{&fenc}]%)%(:%y%)\ \ %r%h%w\ %#Error#%m%#Statusline#\ %F\
" right: buffer num, lines/total, cols/virtual, display percentage
set statusline+=%=buff[%1.3n]\ \ %1.7c%V,%1.7l/%L\ \ [%P])


" compiler and modules configuration
" ----------------------------------

" This assumes that ghc is in your path, if it is not, or you
" wish to use a specific version of ghc, then please change
" the ghc below to a full path to the correct one
au BufEnter *.hs compiler ghc

" For this section both of these should be set to your
" browser and ghc of choice, I used the following
" two vim lines to get those paths:
" :r!which firefox
" :r!whigh ghc
let g:haddock_browser = "/usr/bin/firefox"
let g:ghc = "/usr/bin/ghc"

" configure snips
let g:snips_author = "Edward O'Callaghan"

" configure c.vim compiler flags
let g:C_VimCompilerName = 'clang'
let g:C_CCompiler = 'clang'
let g:C_CplusCompiler = 'clang++'
let g:CFlags = '-O0 -W -Wall -Wextra -pedantic -c'
