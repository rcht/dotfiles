# ---------------------------
# SHELL ENVIRONMENT VARIABLES
# ---------------------------

PROMPT="%(?..%F{red}[%?] )%F{cyan}%d%f%B%F{blue} $ %f%b"

export EDITOR=nvim
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

# -------
# ALIASES
# -------

alias ls='ls --color=auto'
alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'
alias df='df -h'                          
alias free='free -m'                      
alias more=less
alias fucking='sudo'
alias cls='clear'
alias ytdl='yt-dlp'
alias audl='yt-dlp -x --audio-format mp3'
alias vimrc='nvim ~/.config/nvim/init.vim'
alias bashrc='nvim ~/.bashrc && source ~/.bashrc'
alias zshrc='nvim ~/.zshrc && source ~/.zshrc'
alias emurc="nvim ~/.config/alacritty/alacritty.yml"
alias wmrc="nvim ~/.xmonad/xmonad.hs"
alias cowsay='/usr/bin/cowsay'
alias vim=nvim
alias vi="/usr/bin/vim"
alias ls='exa --group-directories-first -a ' 
alias screenshot="flameshot full -c -p ~/Pictures/screenshots/"
alias speedtest='curl https://raw.githubusercontent.com/sivel/speedtest-cli/master/speedtest.py | python -'
alias mskill='killall teams-for-linux;'
alias gst='git status'
alias gl='git log'
alias gpo='git push origin'
alias commit='git commit -m'
alias linedo="xargs -d '\n' -I {}"
alias clock='tty-clock'
alias disconnect="nmcli con down id \"\$(nmcli d | tail -n3 | head -n1 | awk '{for (i = 4; i <= NF; i++) print \$i}')\""
alias wifi-name="nmcli d"
alias l="ls"
alias ll="ls -la"
alias la="ls -la"
alias k="killall wireplumber"

alias gitdf="/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME"

# ----------------------
# FUNCTIONS AND COMMANDS
# ----------------------

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


mkscr()
{
    printf "#!/usr/bin/env bash" > $1
    chmod +x $1
    $EDITOR $1
}

# the file lognumber stores the number of times I've logged in to bash/zsh, and everytime this bashrc is run, that number is updated.
[ ! -e ~/lognumber ] && echo 0 > ~/lognumber
expr $(cat ~/lognumber) + 1 > ~/lognumber
echo -e "$(cat ~/lognumber) logins so far\n"

cal

# -------------------
# SOURCING PLUGINS :(
# -------------------

zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
autoload -Uz compinit && compinit

setopt auto_cd

source ~/zplugins/zsh-autosuggestions/zsh-autosuggestions.zsh

source ~/zplugins/zsh-vi-mode/zsh-vi-mode.zsh

ZVM_CURSOR_STYLE_ENABLED=false

source ~/zplugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
