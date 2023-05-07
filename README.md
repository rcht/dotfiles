# rcht/dotfiles

Configs of programs I use, along with some scripts that I find useful.

Mostly personal backup, nothing too interesting here.

## Dependencies and plugins

### Zsh

#### Plugins

- [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting/)

(for each plugin, clone its git repo to `~/zplugins`)

#### Replaced coreutils

- `ls` replaced by `exa`
- Manpager is `bat` instead of `less`

### Xmonad 

- xmonad>=0.17
- xmonad-contrib>=0.17

### Neovim

#### Plugins (vim-plug)

```
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-compe'
Plug 'SirVer/UltiSnips'
Plug 'jiangmiao/auto-pairs'
Plug 'ThePrimeagen/vim-be-good'
Plug 'rhysd/vim-clang-format'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'ayu-theme/ayu-vim'
Plug 'romgrk/barbar.nvim'
Plug 'nvim-tree/nvim-web-devicons'
Plug 'itchyny/lightline.vim'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'andweeb/presence.nvim'
```

#### Programs and Libraries

- Python `neovim` module (for UltiSnips)
- `clangd`
- `texlab`

### Fonts

- Fira Code
- Hack
- SpaceMono Nerd Font Mono
- Roboto Mono
