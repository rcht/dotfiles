" --------
" Settings
" --------

set number
set relativenumber

set noswapfile

set showcmd
set nowrap
set autoindent
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set scrolloff=10

set nohlsearch
set noerrorbells
set termguicolors " colorizer requires this

set signcolumn=yes
" set colorcolumn=90
syntax on

" ----------
" Remappings
" ----------

let mapleader = " "
set timeoutlen=2000

" I use these so that it's easy to jump into normal mode, I don't type j and k
"together very often. If I have to do it then I'm fucked lol
imap jk <Esc>
imap kj <Esc>

let b:comment_leader = '#'
augroup comment_chars
    autocmd!
    autocmd Filetype c,cpp,rust let b:comment_leader = '//'
    autocmd Filetype sh,python let b:comment_leader = '#'
    autocmd Filetype vim let b:comment_leader = '"'
    autocmd Filetype haskell let b:comment_leader = '--'
augroup END

function! CommentToggle()
    execute ':silent! s/\([^ ]\)/' . escape(b:comment_leader,'\/') . ' \1/'
    execute ':silent! s/^\( *\)' . escape(b:comment_leader,'\/') . ' \?' . escape(b:comment_leader,'\/') . ' \?/\1/'
endfunction

nnoremap <leader>c :call CommentToggle()<CR>j
" map <leader>cu :call CommentToggle()<CR>k
nnoremap <leader>t :MinimapToggle<CR>

" file skeletons
augroup file_template
    autocmd!
    autocmd BufNewFile *.c 0r ~/.config/nvim/templates/new.c
    autocmd BufNewFile *.cpp 0r ~/.config/nvim/templates/new.cpp
    autocmd BufNewFile *.h 0r ~/.config/nvim/templates/new.h
    autocmd BufNewFile *.tex 0r ~/.config/nvim/templates/new.tex
augroup END


nnoremap <leader>mk :make<CR>

" ----------------
" vim-plug plugins
" ----------------

call plug#begin('~/.config/nvim/plugged')

Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-compe'
Plug 'jiangmiao/auto-pairs'
Plug 'ThePrimeagen/vim-be-good'
Plug 'gruvbox-community/gruvbox'
Plug 'ayu-theme/ayu-vim'
Plug 'epmor/hotline-vim'
Plug 'wfxr/minimap.vim', {'do': ':!cargo install --locked code-minimap'}
Plug 'norcalli/nvim-colorizer.lua'

call plug#end()

" -----------------
" Plugin initiation
" -----------------

let ayucolor="dark"
colorscheme ayu
" colorscheme gruvbox
" colorscheme hotline
highlight Normal guibg=none

lua require'colorizer'.setup()

let g:minimap_width = 10


" ---------
" LSP setup 
" ---------

lua << EOF
require'lspconfig'.clangd.setup{} 
require'lspconfig'.hls.setup{}
require'lspconfig'.pylsp.setup{}
require'lspconfig'.texlab.setup{}

vim.o.completeopt = "menuone,noselect"

require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = false;

  source = {
    path = true;
    buffer = true;
    calc = true;
    vsnip = true;
    nvim_lsp = true;
    nvim_lua = true;
    spell = true;
    tags = true;
    snippets_nvim = true;
    treesitter = true;
  };
}

local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
        return true
    else
        return false
    end
end

_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-n>"
  elseif check_back_space() then
    return t "<Tab>"
  else
    return vim.fn['compe#complete']()
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  elseif vim.fn.call("vsnip#jumpable", {-1}) == 1 then
    return t "<Plug>(vsnip-jump-prev)"
  else
    return t "<S-Tab>"
  end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

EOF
