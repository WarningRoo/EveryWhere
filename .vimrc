set nu
set nocompatible
set incsearch
set hlsearch
set showmode
set background=dark
set encoding=utf-8
set laststatus=2
set ruler
set showcmd
set showmatch
set t_Co=256
set ignorecase
set tabstop=8
set shiftwidth=8

set fileencodings=utf-8,ucs-bom,gb18030,gbk,gb2312,cp936
set termencoding=utf-8
set encoding=utf-8

"set compatible
colorscheme delek
"colorscheme elflord
"colorscheme morning
"colorscheme pablo
"colorscheme ron
"colorscheme slate
"colorscheme zellner
"colorscheme peachpuff
"colorscheme default
"colorscheme desert
"colorscheme evening
"colorscheme koehler
"colorscheme blue
"colorscheme murphy
"colorscheme shine
"colorscheme torte
"colorscheme darkblue
"colorscheme industry
"
set autoindent
set cindent

syntax enable
filetype on
filetype plugin on

set tags=tags

"map \p bi(<Esc>ea)<Esc>
"map \c bi{<Esc>ea}<Esc>

"packadd! matchit

"Set mapleader
let mapleader=","

"Fast reloading of the .vimrc
map <silent> <leader>ss :source ~/.vimrc<cr>

"Fast editing of .vimrc
map <silent> <leader>ee :source ~/.vimrc<cr>

"When .vimrc is edited, reload it
autocmd! bufwritepost .vimrc source ~/.vimrc

"Generate tags and cscope.out from FileList.txt (c, cpp, h, hpp)
nmap <C-@> :!find -name "*.c" -o -name "*.cpp" -o -name "*.h" -o -name "*.hpp" > FileList.txt<CR>
        \ :!ctags --c++-kinds=+p --fields=+iaS --extra=+q -L -< FileList.txt<CR>
        \ :!cscope -bkq -i FileList.txt<CR>

"taglist
map <F2> :Tlist<CR>
let Tlist_Show_One_File = 1             "不同时显示多个文件的tag，只显示当前文件的
let Tlist_Exit_OnlyWindow = 1           "如果taglist窗口是最后一个窗口，则退出vim

"NERDTree
map <F4> :NERDTreeMirror<CR>
map <F4> :NERDTreeToggle<CR>
let NERDTreeWinPos="Right"
let NERDTreeShowBookmarks=1

"CSCOPE
if has("cscope")
	set csprg=/usr/bin/cscope
	set csto=0
	set cst
	set nocsverb
	" add any database in current directory
	if filereadable("cscope.out")
		cs add cscope.out
	" else add database pointed to by environment
	elseif $CSCOPE_DB != ""
		cs add $CSCOPE_DB
	endif
	set csverb
endif

nmap <C-\>s :cs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>g :cs find g <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>c :cs find c <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>t :cs find t <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>e :cs find e <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-\>d :cs find d <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
