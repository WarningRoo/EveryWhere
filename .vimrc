" ./vim/pack/SI/start/

" BASE Configuration
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
set autoindent
set cindent

syntax enable
filetype on
filetype plugin on

"packadd! matchit

"resize window
:nnoremap <silent> <C-up> :resize -2<CR>
:nnoremap <silent> <C-down> :resize +2<CR>
:nnoremap <silent> <C-left> :vertical resize -2<CR>
:nnoremap <silent> <C-right> :vertical resize +2<CR>

" theme
" [install_from] git clone https://github.com/morhetz/gruvbox.git
set bg=dark
autocmd vimenter * ++nested colorscheme gruvbox
"set compatible
"colorscheme delek
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

"-------------------------------------------------------------------------------------------------
" ctags / cscope
" [install_from]
" sudo apt-get install ctags
" sudo apt-get install cscope
" Generate tags and cscope.out from FileList.txt (c, cpp, h, hpp)
nmap <C-@> :!find -name "*.c" -o -name "*.cpp" -o -name "*.h" -o -name "*.hpp" > FileList.txt<CR>
        \ :!ctags --c++-kinds=+p --fields=+iaS --extra=+q -L -< FileList.txt<CR>
        \ :!cscope -RbuUq -i FileList.txt<CR>

set tags=tags

" CSCOPE
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

nmap <C-_>s :cs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>g :cs find g <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>c :cs find c <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>t :cs find t <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>e :cs find e <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-_>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
nmap <C-_>d :cs find d <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>a :cs find a <C-R>=expand("<cword>")<CR><CR>

" Using 'CTRL-spacebar' then a search type makes the vim window
" split horizontally, with search result displayed in
" the new window.

"nmap <C-Space>s :scs find s <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space>g :scs find g <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space>c :scs find c <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space>t :scs find t <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space>e :scs find e <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space>f :scs find f <C-R>=expand("<cfile>")<CR><CR>
"nmap <C-Space>i :scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
"nmap <C-Space>d :scs find d <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space>a :scs find a <C-R>=expand("<cword>")<CR><CR>

" Hitting CTRL-space *twice* before the search type does a vertical
" split instead of a horizontal one

"nmap <C-Space><C-Space>s :vert scs find s <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space><C-Space>g :vert scs find g <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space><C-Space>c :vert scs find c <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space><C-Space>t :vert scs find t <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space><C-Space>e :vert scs find e <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space><C-Space>i :vert scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
"nmap <C-Space><C-Space>d :vert scs find d <C-R>=expand("<cword>")<CR><CR>
"nmap <C-Space><C-Space>a :vert scs find a <C-R>=expand("<cword>")<CR><CR>

"-------------------------------------------------------------------------------------------------

" taglist
" [install_from]
" git clone https://github.com/yegappan/taglist
map <F2> :Tlist<CR>
let Tlist_Show_One_File = 1             "不同时显示多个文件的tag，只显示当前文件的
let Tlist_Exit_OnlyWindow = 1           "如果taglist窗口是最后一个窗口，则退出vim

"-------------------------------------------------------------------------------------------------

" NERDTree
" [install_from]
" git clone https://github.com/preservim/nerdtree.git
map <F4> :NERDTreeMirror<CR>
map <F4> :NERDTreeToggle<CR>
let NERDTreeWinPos="Right"
let NERDTreeShowBookmarks=1

"-------------------------------------------------------------------------------------------------

" YCM
" [install_from]
" git clone https://github.com/ycm-core/YouCompleteMe.git
" git submodule update --init --recursive
if filereadable(expand('~/.vim/pack/SI/start/YouCompleteMe/autoload/youcompleteme.vim'))
	let g:ycm_key_list_select_completion = ['<TAB>', '<Down>']
	let g:ycm_key_list_previous_completion = ['<S-TAB>', '<Up>']
	let g:ycm_key_list_stop_completion = ['<C-y>']
	let g:ycm_key_invoke_completion = '<C-Space>'
	let g:ycm_autoclose_preview_window_after_completion = 1
	let g:ycm_autoclose_preview_window_after_insertion = 1
endif

"-------------------------------------------------------------------------------------------------

" fzf
" [install_from]
" sudo apt-get install fzf
" git clone https://github.com/junegunn/fzf.git

" fzf.vim
" [install_from]
" git clone https://github.com/junegunn/fzf.vim.git

"-------------------------------------------------------------------------------------------------
