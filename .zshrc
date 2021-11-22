export PATH=$HOME/bin:/usr/local/bin:$PATH

export ZSH="/home/rachit/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git vi-mode zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

export EDITOR=nvim

alias ls='ls --color=auto'
alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias more=less
alias fucking='sudo'
alias cls='clear'
alias ytdl='youtube-dl'
alias audl='youtube-dl -x --audio-format mp3'
alias vimrc='nvim ~/.config/nvim/init.vim'
alias bashrc='nvim ~/.bashrc && source ~/.bashrc'
alias emurc="nvim ~/.config/alacritty/alacritty.yml"
alias wmrc="nvim ~/.xmonad/xmonad.hs"
alias cowsay='/usr/bin/cowsay'
alias vim=nvim
alias ls='exa --group-directories-first -a ' 
alias screenshot="flameshot full -c -p ~/Pictures/screenshots/"
alias speedtest='curl https://raw.githubusercontent.com/sivel/speedtest-cli/master/speedtest.py | python -'
alias mskill='killall teams-for-linux; killall alacritty'
alias gst='git status'
alias gl='git log'
alias gpo='git push origin'
alias commit='git commit -m'

alias gitdf="/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME"

ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

function zle-line-init zle-keymap-select {
    RPS1="${${KEYMAP/vicmd/-- NORMAL --}/(main|viins)/-- INSERT --}"
    RPS2=$RPS1
    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

cal
