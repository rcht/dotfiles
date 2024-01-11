" --------
" Settings
" --------


set number
set relativenumber

set noswapfile

set showcmd
set autoindent
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set scrolloff=10
set cursorline

set title
set nohlsearch
set noerrorbells
set termguicolors " colorizer requires this

set signcolumn=yes
set mouse=
syntax on

" ----------
" Remappings
" ----------

let mapleader = " "
set timeoutlen=2000

" I use these so that it's easy to jump into normal mode, I don't type j and k
" together very often. If I have to do it then I'm fucked lol
imap jk <Esc>
imap kj <Esc>

let b:comment_leader = '#'
augroup comment_chars
    autocmd!
    autocmd Filetype c,cpp,rust,javascript let b:comment_leader = '//'
    autocmd Filetype vim let b:comment_leader = '"'
    autocmd Filetype haskell let b:comment_leader = '--'
    autocmd Filetype tex let b:comment_leader = '%'
augroup END

function! CommentToggle()
    execute ':silent! s/\([^ ]\)/' . escape(b:comment_leader,'\/') . ' \1/'
    execute ':silent! s/^\( *\)' . escape(b:comment_leader,'\/') . ' \?' . escape(b:comment_leader,'\/') . ' \?/\1/'
endfunction

nnoremap <leader>b :Files .<CR>
nnoremap <leader>c :call CommentToggle()<CR>j
nnoremap <leader>fb :ClangFormat<CR>
" yank buffer  
nnoremap <leader>yb :%w !xclip -i -sel c<CR> 

" file skeletons
augroup file_template
    autocmd!
    autocmd BufNewFile *.c 0r ~/.config/nvim/templates/new.c
    autocmd BufNewFile *.cpp 0r ~/.config/nvim/templates/new.cpp
    autocmd BufNewFile *.h 0r ~/.config/nvim/templates/new.h
    autocmd BufNewFile *.tex 0r ~/.config/nvim/templates/new.tex
augroup END

" 

nnoremap <leader>mk :make<CR>

" let g:presence_log_level="debug"

" ----------------
" vim-plug plugins
" ----------------

call plug#begin('~/.config/nvim/plugged')

Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-compe'
Plug 'SirVer/UltiSnips'
Plug 'jiangmiao/auto-pairs'
Plug 'ThePrimeagen/vim-be-good'
Plug 'rhysd/vim-clang-format'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Plug 'gruvbox-community/gruvbox'
" Plug 'talha-akram/noctis.nvim'
Plug 'ayu-theme/ayu-vim'
Plug 'romgrk/barbar.nvim'
Plug 'nvim-tree/nvim-web-devicons'
" Plug 'sheerun/vim-polyglot'
Plug 'itchyny/lightline.vim'
Plug 'norcalli/nvim-colorizer.lua'
" gotta rizz em up
" Plug 'andweeb/presence.nvim'

call plug#end()

" -----------------
" Plugin initiation
" -----------------

let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

colorscheme ayu 
" highlight Normal guibg=none
" highlight clear SignColumn

lua require'colorizer'.setup()

let g:lightline = {
      \ 'colorscheme': 'molokai',
      \ }

" Move to previous/next
nnoremap <leader>nj <Cmd>BufferPrevious<CR>
nnoremap <leader>nk <Cmd>BufferNext<CR>

" Close buffer
nnoremap <leader>wq <Cmd>BufferClose<CR>
" Restore buffer
nnoremap <leader>u <Cmd>BufferRestore<CR>

" ---------
" LSP setup 
" ---------

lua << EOF
require'lspconfig'.clangd.setup{} 
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
    calc = false;
    vsnip = false;
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

for _, group in ipairs(vim.fn.getcompletion("@lsp", "highlight")) do
   vim.api.nvim_set_hl(0, group, {})
end

-- vim.api.nvim_set_hl(0, 'Identifier', { bold=true })
vim.api.nvim_set_hl(0, 'Comment', { italic=true, fg="Grey" })
vim.api.nvim_set_hl(0, 'String', { italic=true, fg="#B8CC52" })
-- vim.api.nvim_set_hl(0, 'Constant', { fg="Red" })

EOF

