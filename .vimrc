"-------------------------------------------------------------------------------------------------
" VIM-plug
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
let plug_vim = data_dir . '/autoload/plug.vim'
let plug_dir = data_dir . '/plugged'

if empty(glob(plug_vim))
	silent execute '!curl -fLo ' . plug_vim . ' --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin(plug_dir)

Plug 'junegunn/vim-plug'
Plug 'Yggdroot/LeaderF', { 'do': ':LeaderfInstallCExtension' }
Plug 'tomasr/molokai'
Plug 'preservim/nerdtree'
Plug 'vim-scripts/taglist.vim'
Plug 'vim-airline/vim-airline'
Plug 'atelierbram/vim-colors_atelier-schemes'
Plug 'ycm-core/YouCompleteMe', { 'do': './install.py' }

" We have to do these for gtags manually:
" 1. Download the newest GNU-Global soft from "https://ftp.gnu.org/pub/gnu/global/"
" 2. Configure and INSTALL, see global/INSTALL
" 3. mkdir -p ...plugged/gtags/plugin | cp global-xxx/*.vim " ...plugged/gtags/plugin
Plug plug_dir.'/gtags'

call plug#end()

"-------------------------------------------------------------------------------------------------

" Strip blank at the end of line
autocmd BufWritePre * :%s/\s\+$//e

" BASE Configuration
set nu
set nocompatible
set incsearch
set hlsearch
set cursorline
set cursorcolumn
set showmode
set background=dark
set encoding=utf-8
set laststatus=2
set ruler
set nowrap
set autowrite
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
set history=200

syntax on
filetype on
filetype plugin on
filetype plugin indent on

packadd! matchit
runtime! ftplugin/man.vim

"resize window
nnoremap <silent> <C-up> :resize +2<CR>
nnoremap <silent> <C-down> :resize -2<CR>
nnoremap <silent> <C-left> :vertical resize -2<CR>
nnoremap <silent> <C-right> :vertical resize +2<CR>

"-------------------------------------------------------------------------------------------------
" gtags
nmap <C-@> :call MakeGTAGSFiles()<CR>

let s:prj_file_tag = expand('./.cache')
function! MakeGTAGSFiles()
	if isdirectory(s:prj_file_tag)
		Leaderf gtags --update
	else
		echo "Make sure you've 'mkdir .cache' in your project root directory!"
	endif
endfunction

map <C-n> :cn<CR>
map <C-p> :cp<CR>
map <C-_> :GtagsCursor<CR>
"nmap <C-=>c :Gtags <C-R>=expand("<cword>")<CR><CR>
"nmap <C-q>c :Gtags -r <C-R>=expand("<cword>")<CR><CR>

"-------------------------------------------------------------------------------------------------

let g:Lf_RootMarkers = [s:prj_file_tag]
let g:Lf_GtagsStoreInRootMarker = 1

" don't show the help in normal mode
let g:Lf_HideHelp = 0
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
noremap <leader>fs :<C-U><C-R>=printf("Leaderf! gtags %s", "")<CR><CR>
noremap <leader>fr :<C-U><C-R>=printf("Leaderf! gtags -r %s --auto-jump", expand("<cword>"))<CR><CR>
noremap <leader>fd :<C-U><C-R>=printf("Leaderf! gtags -d %s --auto-jump", expand("<cword>"))<CR><CR>
noremap <leader>fg :<C-U><C-R>=printf("Leaderf! gtags -g %s --auto-jump", expand("<cword>"))<CR><CR>
noremap <leader>fb :<C-U><C-R>=printf("Leaderf buffer %s", "")<CR><CR>
noremap <leader>fm :<C-U><C-R>=printf("Leaderf mru %s", "")<CR><CR>
noremap <leader>fl :<C-U><C-R>=printf("Leaderf line %s", "")<CR><CR>

"noremap <C-B> :<C-U><C-R>=printf("Leaderf! rg --current-buffer -e %s ", expand("<cword>"))<CR>
"noremap <C-F> :<C-U><C-R>=printf("Leaderf! rg -e %s ", expand("<cword>"))<CR>
" search visually selected text literally
xnoremap gf :<C-U><C-R>=printf("Leaderf! rg -F -e %s ", leaderf#Rg#visual())<CR>
noremap go :<C-U>Leaderf! rg --recall<CR>

" should use `Leaderf gtags --update` first
let g:Lf_GtagsAutoGenerate = 0
let g:Lf_GtagsGutentags = 0
let g:Lf_Gtagslabel = 'pygments'
noremap <leader>fo :<C-U><C-R>=printf("Leaderf! gtags --recall %s", "")<CR><CR>
noremap <leader>fn :<C-U><C-R>=printf("Leaderf gtags --next %s", "")<CR><CR>
noremap <leader>fp :<C-U><C-R>=printf("Leaderf gtags --previous %s", "")<CR><CR>

"-------------------------------------------------------------------------------------------------

" taglist
map <F2> :Tlist<CR>
let Tlist_Show_One_File = 1             "不同时显示多个文件的tag，只显示当前文件的
let Tlist_Exit_OnlyWindow = 1           "如果taglist窗口是最后一个窗口，则退出vim

"-------------------------------------------------------------------------------------------------

" NERDTree
map <leader>nf :NERDTreeFind<CR>
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

" airline

"-------------------------------------------------------------------------------------------------

" theme
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
"colorscheme Atelier_CaveDark
"colorscheme Atelier_CaveLight
"colorscheme Atelier_DuneDark
"colorscheme Atelier_DuneLight
"colorscheme Atelier_EstuaryDark
"colorscheme Atelier_EstuaryLight
"colorscheme Atelier_ForestDark
"colorscheme Atelier_ForestLight
"colorscheme Atelier_HeathDark
"colorscheme Atelier_HeathLight
"colorscheme Atelier_LakesideDark
"colorscheme Atelier_LakesideLight
"colorscheme Atelier_PlateauDark
"colorscheme Atelier_PlateauLight
"colorscheme Atelier_SavannaDark
"colorscheme Atelier_SavannaLight
"colorscheme Atelier_SeasideDark
"colorscheme Atelier_SeasideLight
"colorscheme Atelier_SulphurpoolDark
"colorscheme Atelier_SulphurpoolLight
colorscheme molokai
