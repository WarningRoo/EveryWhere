" ./vim/pack/SI/start/

" Strip the blank in the end of line
autocmd BufWritePre * :%s/\s\+$//e

" BASE Configuration
set nu
set nocompatible
set incsearch
set hlsearch
set showmode
"set background=dark
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
:nnoremap <silent> <C-up> :resize +2<CR>
:nnoremap <silent> <C-down> :resize -2<CR>
:nnoremap <silent> <C-left> :vertical resize -2<CR>
:nnoremap <silent> <C-right> :vertical resize +2<CR>

"-------------------------------------------------------------------------------------------------
" gtags
" [install_from]
" sudo apt-get install global
"nmap <C-@> :Leaderf gtags --update<CR>
nmap <C-@> :call CacheFileWarning()<CR>
function! CacheFileWarning()
	if isdirectory(expand("./.cache"))
		Leaderf gtags --update
	else
		echo "Please mkdir one .cache file in your project root directory!"
	endif
endfunction

map <C-n> :cn<CR>
map <C-p> :cp<CR>
map <C-_> :GtagsCursor<CR>
"nmap <C-=>c :Gtags <C-R>=expand("<cword>")<CR><CR>
"nmap <C-q>c :Gtags -r <C-R>=expand("<cword>")<CR><CR>

"-------------------------------------------------------------------------------------------------

let s:prj_file_tag = expand('./.cache')
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

let g:Lf_WildIgnore = {
	\ 'dir': ['.svn','.git','.hg'],
	\ 'file': ['*.sw?','~$*','*.bak','*.exe','*.o','*.so','*.py[co]']
	\}
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

" gutentags
" [install_from]
" git clone https://github.com/ludovicchabant/vim-gutentags.git
" helptags ~/.vim/pack/SI/start/vim-gutentags/doc
"
"let g:gutentags_project_root = ['.project', '.svn', '.git']
"let g:gutentags_ctags_tagfile = 'GTAGS'
"let g:gutentags_modules = ['gtags_cscope']
"let g:gutentags_define_advanced_commands = 1
"let g:gutentags_cache_dir = expand(g:Lf_CacheDirectory.'/.cache/gtags')

"-------------------------------------------------------------------------------------------------

" air line
" git clone https://github.com/vim-airline/vim-airline
" :helptags ~/.vim/pack/dist/start/vim-airline/doc

"-------------------------------------------------------------------------------------------------

" theme
" [install_from]
" git clone https://github.com/atelierbram/vim-colors_atelier-schemes.git
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
