"-------------------------------------------------------------------------------------------------
" Current workflow is:
" 1. Change to the project's root directory and 'mkdir -p .cache'
" 2. Open vim directly in the project's root directory and execute '<C-@>' to
"    generate the index files.
" 3. That's it.
" ATTENTION:
" a. If you open Vim in a subdirectory below the project's root directory.
"    Don't worry; the following code will automatically search upwards to find
"    the directory containing '.cache/' and use it as the project's root.
" b. Of course, if you haven't created .cache/ yet and the search in 'a' leads
"    to your home directory or the root directory, Vim will automatically use
"    the current directory as the project's root.
"-------------------------------------------------------------------------------------------------

let g:cache_dir = fnamemodify(finddir('.cache', ';'), ':p')
let g:root_dir = fnamemodify(g:cache_dir, ':h:h')

if g:root_dir == expand('~') || empty(g:root_dir)
	let g:root_dir = getcwd()
endif

"-------------------------------------------------------------------------------------------------
" VIM-plug
let data_dir = expand(has('nvim') ? stdpath('data') . '/site' : '~/.vim')
let plug_vim = data_dir . '/autoload/plug.vim'
let plug_dir = data_dir . '/plugged'

if empty(glob(plug_vim))
	silent execute '!curl -fLo ' . plug_vim . ' --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin(plug_dir)

Plug 'junegunn/vim-plug'

Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
"Plug 'tpope/vim-surround'
Plug 'Yggdroot/LeaderF', { 'do': ':LeaderfInstallCExtension' }
Plug 'preservim/nerdtree'
Plug 'vim-scripts/taglist.vim'
Plug 'ycm-core/YouCompleteMe', { 'do': './install.py' }

" +++++++++++++++++++++++++++++++++++++++++++
" LSP
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
" LSP
" +++++++++++++++++++++++++++++++++++++++++++

" We have to do these for gtags manually:
" 1. Download the newest GNU-Global soft from "https://ftp.gnu.org/pub/gnu/global/"
" 2. Configure and INSTALL, see global/INSTALL
" 3. mkdir -p ...plugged/gtags/plugin | cp global/*.vim ...plugged/gtags/plugin
Plug plug_dir.'/gtags'

" +++++++++++++++++++++++++++++++++++++++++++
" colorscheme
Plug 'ghifarit53/tokyonight-vim'
Plug 'tomasr/molokai'
Plug 'atelierbram/vim-colors_atelier-schemes'
Plug 'morhetz/gruvbox'
Plug 'embark-theme/vim', { 'as': 'embark', 'branch': 'main' }
" +++++++++++++++++++++++++++++++++++++++++++

call plug#end()

"-------------------------------------------------------------------------------------------------
" BASE Configuration (no plugin)
set encoding=utf-8
set fileencodings=utf-8,ucs-bom,gb18030,gbk,gb2312,cp936
set termencoding=utf-8
set nu
set nocompatible
set hlsearch incsearch
set cursorline cursorcolumn
set showmode
set background=dark
set laststatus=2
set ruler
set nowrap
set autowrite autoread
set showcmd
set showmatch
set ignorecase smartcase
set tabstop=4
set shiftwidth=4
set autoindent
set history=200
set clipboard=unnamed
set t_Co=256
set termguicolors
set colorcolumn=80
set splitbelow splitright
set nobackup noswapfile
set mouse=a
"set confirm

"set foldmethod=syntax
"set nofoldenable

execute 'set path+=' . g:root_dir . '/**'

set wildmenu
set completeopt=menuone,preview,noselect
set omnifunc=syntaxcomplete#Complet
set shortmess+=c
set cpt+=kspell

syntax on
filetype on
filetype plugin on
filetype plugin indent on

packadd! matchit
runtime! ftplugin/man.vim

" [INSERT MODE] <C-v><Alt-h> -> h
" [INSERT MODE] <C-v><Ctrl-h> -> h
nnoremap <silent> <C-h> <C-w>h
nnoremap <silent> <C-j> <C-w>j
nnoremap <silent> <C-k> <C-w>k
nnoremap <silent> <C-l> <C-w>l

"resize window
nnoremap <silent> [A :resize +4<CR>
nnoremap <silent> [B :resize -4<CR>
nnoremap <silent> [D :vertical resize -4<CR>
nnoremap <silent> [C :vertical resize +4<CR>
" <C-w>= -> Resize window automally

"inoremap < <><left>
"noremap! ( ()<left>
"noremap! [ []<left>
"noremap! { {}<left>
"noremap! " ""<left>
"noremap! ' ''<left>

" .vimrc fast edit/save
nnoremap <leader>ev :edit $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

"nnoremap <leader>' ea'<esc>bi'<esc>
"nnoremap <leader>" ea"<esc>bi"<esc>
"nnoremap <leader>( ea)<esc>bi(<esc>
"nnoremap <leader>[ ea]<esc>bi[<esc>
"nnoremap <leader>{ ea}<esc>bi{<esc>

"autocmd FileType c      nnoremap <buffer> <localleader>c I//<esc>
"autocmd FileType vim    nnoremap <buffer> <localleader>c I"<esc>
"autocmd FileType python nnoremap <buffer> <localleader>c I#<esc>

cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Strip blank at the end of line
autocmd BufWritePre * :%s/\s\+$//e

" Open/Close Quickfix with one key-assign
function! ToggleQuickFix()
	if empty(filter(getwininfo(), 'v:val.quickfix'))
		copen
	else
		cclose
	endif
endfunction
nnoremap <silent> <C-q> :call ToggleQuickFix()<CR>


"-------------------------------------------------------------------------------------------------
" gtags
" Enable the gtags to reference the GTAGS file generated by Leaderf gtags.
let $GTAGSROOT = g:root_dir
let $GTAGSDBPATH = g:cache_dir . '/.LfGtags'

" If you make empty GTAGS or Gtags doesn't figure out .py/.vim... file. JUST
" Check EXUBERANT_CTAGS in pygments_parser.py and Ensure the ctags is exactly
" located there.
" You may need `ln` one new ctags or reinstall `global`(./configure | make |
" sudo make install)
let $GTAGSLABEL = 'pygments'
let $GTAGSCONF = '/usr/local/share/gtags/gtags.conf'

" ATTENTION:
"   Run this command in the project's root directory, which should have a
"   .cache/ directory; otherwise, you'll run into an error.
nnoremap <C-@> :call MakeGTAGSFiles()<CR>
function! MakeGTAGSFiles()
	if isdirectory(g:cache_dir)
		Leaderf gtags --update
	else
		echo "Make sure you've 'mkdir .cache' in your project root directory!"
	endif
endfunction

nnoremap <C-n> :cnext<CR>
nnoremap <C-p> :cprev<CR>
nnoremap <C-_> :GtagsCursor<CR>
"nnoremap <C-=>c :Gtags <C-R>=expand("<cword>")<CR><CR>
"nnoremap <C-q>c :Gtags -r <C-R>=expand("<cword>")<CR><CR>

"-------------------------------------------------------------------------------------------------

let g:Lf_RootMarkers = ['.cache']
let g:Lf_GtagsStoreInRootMarker = 1
let g:Lf_Gtagslabel = 'pygments'

" don't show the help in normal mode
let g:Lf_HideHelp = 1
let g:Lf_UseCache = 0
let g:Lf_UseVersionControlTool = 0
let g:Lf_IgnoreCurrentBufferName = 1
" popup mode
let g:Lf_WindowPosition = 'popup'
let g:Lf_StlSeparator = { 'left': "\ue0b0", 'right': "\ue0b2", 'font': "DejaVu Sans Mono for Powerline" }
let g:Lf_WildIgnore = {
			\ 'dir': ['.svn','.git','.hg'],
			\ 'file': ['*.sw?','~$*','*.bak','*.exe','*.o','*.so','*.py[co]']
			\}

" MOST inuse
let g:Lf_ShortcutF = "<leader>ff"
noremap <leader>ft :<C-U><C-R>=printf("Leaderf bufTag %s", "")<CR><CR>
noremap <leader>fa :<C-U><C-R>=printf("Leaderf  gtags --result ctags-x %s", "")<CR><CR>
noremap <leader>fr :<C-U><C-R>=printf("Leaderf! gtags --result ctags-x -r %s --auto-jump", expand("<cword>"))<CR><CR>
noremap <leader>fs :<C-U><C-R>=printf("Leaderf! gtags --result ctags-x -s %s --auto-jump", expand("<cword>"))<CR><CR>
noremap <leader>fd :<C-U><C-R>=printf("Leaderf! gtags --result ctags-x -d %s --auto-jump", expand("<cword>"))<CR><CR>
noremap <leader>fg :<C-U><C-R>=printf("Leaderf! gtags --result ctags-x -g %s --auto-jump", expand("<cword>"))<CR><CR>

noremap <leader>fo :<C-U><C-R>=printf("Leaderf! gtags --recall %s", "")<CR><CR>
noremap <leader>fn :<C-U><C-R>=printf("Leaderf gtags --next %s", "")<CR><CR>
noremap <leader>fp :<C-U><C-R>=printf("Leaderf gtags --previous %s", "")<CR><CR>

noremap <leader>fb :<C-U><C-R>=printf("Leaderf buffer %s", "")<CR><CR>
noremap <leader>fm :<C-U><C-R>=printf("Leaderf mru %s", "")<CR><CR>
noremap <leader>fl :<C-U><C-R>=printf("Leaderf line %s", "")<CR><CR>

" rg
let g:Lf_RgConfig = [
			\ "--max-columns=150",
			\ "--type-add web:*.{html,css,js}*",
			\ "--glob=!git/*",
			\ "--hidden"
			\ ]

" Search in ALL-files / current-file
noremap <leader>ra :<C-U><C-R>=printf("Leaderf rg -e %s ", expand("<cword>"))<CR>
noremap <leader>rc :<C-U><C-R>=printf("Leaderf rg -F --current-buffer -e %s ", expand("<cword>"))<CR>
noremap go :<C-U>Leaderf! rg --recall<CR>

"noremap <leader>ro :<C-U><C-R>=printf("Leaderf! rg --append -e %s ", expand("<cword>"))<CR>
" search visually selected text literally, don't quit LeaderF after accepting an entry
"xnoremap gf :<C-U><C-R>=printf("Leaderf! rg -F --stayOpen -e %s ", leaderf#Rg#visual())<CR>
" search word under cursor, use --heading
"noremap <C-H> :<C-U><C-R>=printf("Leaderf! rg -e %s --heading -C3 ", expand("<cword>"))<CR>
" search word under cursor literally in all listed buffers
"noremap <C-D> :<C-U><C-R>=printf("Leaderf! rg -F --all-buffers -e %s ", expand("<cword>"))<CR>

" search word under cursor in *.h and *.cpp files.
"noremap <Leader>a :<C-U><C-R>=printf("Leaderf! rg -e %s -g *.h -g *.cpp", expand("<cword>"))<CR>
"noremap <Leader>a :<C-U><C-R>=printf("Leaderf! rg -e %s -g *.{h,cpp}", expand("<cword>"))<CR>
"noremap <Leader>b :<C-U><C-R>=printf("Leaderf! rg -e %s -t cpp -t java", expand("<cword>"))<CR>
"noremap <Leader>c :<C-U><C-R>=printf("Leaderf! rg -e %s -t cpp -g !*.hpp", expand("<cword>"))<CR>

"-------------------------------------------------------------------------------------------------

" taglist
nnoremap <F2> :Tlist<CR>
let Tlist_Show_One_File = 1             "不同时显示多个文件的tag，只显示当前文件的
let Tlist_Exit_OnlyWindow = 1           "如果taglist窗口是最后一个窗口，则退出vim

"-------------------------------------------------------------------------------------------------

" NERDTree
nnoremap <leader>n :NERDTreeFocus<CR>
"nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>

let NERDTreeWinPos="Right"
let NERDTreeShowBookmarks=1

" Exit Vim if NERDTree is the only window remaining in the only tab.
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif

" Close the tab if NERDTree is the only window remaining in it.
autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif

" If another buffer tries to replace NERDTree, put it in the other window, and bring back NERDTree.
autocmd BufEnter * if bufname('#') =~ 'NERD_tree_\d\+' && bufname('%') !~ 'NERD_tree_\d\+' && winnr('$') > 1 |
			\ let buf=bufnr() | buffer# | execute "normal! \<C-W>w" | execute 'buffer'.buf | endif

"-------------------------------------------------------------------------------
" Open file in reoute server
" Function to open the file or NERDTree or netrw.
"   Returns: 1 if either file explorer was opened; otherwise, 0.
function! s:OpenFileOrExplorer(...)
	if a:0 == 0 || a:1 == ''
		NERDTree
	elseif filereadable(a:1)
		execute 'edit '.a:1
		return 0
	elseif a:1 =~? '^\(scp\|ftp\)://' " Add other protocols as needed.
		execute 'Vexplore '.a:1
	elseif isdirectory(a:1)
		execute 'NERDTree '.a:1
	endif
	return 1
endfunction

" Auto commands to handle OS commandline arguments
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc()==1 && !exists('s:std_in') | if <SID>OpenFileOrExplorer(argv()[0]) | wincmd p | enew | wincmd p | endif | endif

" Command to call the OpenFileOrExplorer function.
command! -n=? -complete=file -bar Edit :call <SID>OpenFileOrExplorer('<args>')

" Command-mode abbreviation to replace the :edit Vim command.
cnoreabbrev e Edit
"-------------------------------------------------------------------------------

"-------------------------------------------------------------------------------------------------
" YCM
if filereadable(expand(plug_dir.'/YouCompleteMe/plugin/youcompleteme.vim'))
	let g:ycm_key_list_select_completion = ['<TAB>', '<Down>']
	let g:ycm_key_list_previous_completion = ['<S-TAB>', '<Up>']
	let g:ycm_key_list_stop_completion = ['<C-y>']
	let g:ycm_key_invoke_completion = '<C-Space>'
	let g:ycm_autoclose_preview_window_after_completion = 1
	let g:ycm_autoclose_preview_window_after_insertion = 1
endif

"-------------------------------------------------------------------------------------------------

"if executable('pylsp')
"	au User lsp_setup call lsp#register_server({
"				\ 'name': 'pylsp',
"				\ 'cmd': {server_info->['pylsp']},
"				\ 'allowlist': ['python'],
"				\ })
"endif
"
if executable('clangd')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'clangd',
        \ 'cmd': {server_info->['clangd', '-background-index']},
        \ 'whitelist': ['c', 'cpp', 'objc', 'objcpp'],
        \ })
endif

function! s:on_lsp_buffer_enabled() abort
	setlocal omnifunc=lsp#complete
	setlocal signcolumn=yes
	if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
	nmap <buffer> <leader>gd <plug>(lsp-definition)
	nmap <buffer> <leader>gs <plug>(lsp-document-symbol-search)
	nmap <buffer> <leader>gS <plug>(lsp-workspace-symbol-search)
	nmap <buffer> <leader>gr <plug>(lsp-references)
	nmap <buffer> <leader>gi <plug>(lsp-implementation)
	nmap <buffer> <leader>gt <plug>(lsp-type-definition)
	nmap <buffer> <leader>rn <plug>(lsp-rename)
	nmap <buffer> [g <plug>(lsp-previous-diagnostic)
	nmap <buffer> ]g <plug>(lsp-next-diagnostic)
	nmap <buffer> K <plug>(lsp-hover)
"	nnoremap <buffer> <expr><c-f> lsp#scroll(+4)
"	nnoremap <buffer> <expr><c-d> lsp#scroll(-4)

	let g:lsp_format_sync_timeout = 1000
	autocmd! BufWritePre *.rs,*.go call execute('LspDocumentFormatSync')

	" refer to doc to add more commands
endfunction

augroup lsp_install
	au!
	" call s:on_lsp_buffer_enabled only for languages that has the server registered.
	autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

"-------------------------------------------------------------------------------------------------

let g:tokyonight_style = 'storm'
colorscheme embark

"-------------------------------------------------------------------------------------------------
" search
execute 'set grepprg=grep\ -R\ -n\ $*\ ' .
			\ '\ --exclude={a.out,*.o,*.a}\ ' .
			\ '\ --exclude-dir={.svn,.git,.cache}\ ' .
			\ '\ /dev/null'

" Search with operator
nnoremap <leader>gl :set operatorfunc=GrepOperatorLocal<CR>g@
nnoremap <leader>gg :set operatorfunc=GrepOperator<CR>g@
vnoremap <leader>gl :<C-u>call GrepOperatorLocal(visualmode())<CR>
vnoremap <leader>gg :<C-u>call GrepOperator(visualmode())<CR>

function! GrepOperatorLocal(type)
	let l:saved_unnamed_register = @@

	if a:type ==# 'v'
		normal! `<v`>y
	elseif a:type ==# 'char'
		normal! `[v`]y
	else
		return
	endif

	silent execute "grep! " . shellescape(expand(@@)) . " " . shellescape(expand('%'))
	copen
	redraw!

	let @@ = saved_unnamed_register
endfunction

function! GrepOperator(type)
	let l:saved_unnamed_register = @@

	if a:type ==# 'v'
		normal! `<v`>y
	elseif a:type ==# 'char'
		normal! `[v`]y
	else
		return
	endif

	silent execute "grep! " . shellescape(expand(@@)) . " " . shellescape(g:root_dir)
	copen
	redraw!

	let @@ = saved_unnamed_register
endfunction

" Search with command
command! -nargs=+ Grep call GrepCommand(<f-args>)
function! GrepCommand(...)
	if a:0 ==# 1 || a:0 ==# 2
		let l:pattern = a:1
	else
		echom 'Usage: :Grep pattern [path]'
	end

	if a:0 ==# 2 && a:2 ==# '%'
		silent execute "grep! " . l:pattern . ' ' . expand('%')
	else
		silent execute "grep! " . l:pattern . ' ' . g:root_dir
	endif

	copen
	redraw!
endfunction
