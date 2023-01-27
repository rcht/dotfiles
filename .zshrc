# ---------------------------
# SHELL ENVIRONMENT VARIABLES
# ---------------------------

PROMPT="%(?..%F{red}[%?] )%F{cyan}%d%f%B%F{blue} (0_o)--> %f%b"

export EDITOR=nvim
export PATH="/home/rachit/.local/bin:/home/rachit/.cargo/bin:$PATH"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
source "$HOME/.cargo/env"

# -------
# ALIASES
# -------
alias py='python'
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
alias pkhex='WINEARCH=win32 WINEPREFIX=~/wine wine ~/wine/exe/PkHaX.exe'
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
alias z="zathura"
alias nb="newsboat"
alias zen="~/.local/bin/zentile_linux_amd64 &"
alias waclist="xsetwacom list devices"

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
    printf "#!/usr/bin/env bash" > "$1"
    chmod +x "$1"
    $EDITOR "$1"
}
mkpy()
{
    printf "#!/usr/bin/env python" > "$1"
    chmod +x "$1"
    $EDITOR "$1"
}
mkc()
{
    printf "#!/usr/bin/tcc -run" > "$1"
    chmod +x "$1"
    $EDITOR "$1"
}
# stopwatch and timer (found on superuser)

countdown() {
    start="$(( $(date +%s) + $1))"
    while [ "$start" -ge $(date +%s) ]; do
        ## Is this more than 24h away?
        days="$(($(($(( $start - $(date +%s) )) * 1 )) / 86400))"
        time="$(( $start - `date +%s` ))"
        printf '%s day(s) and %s\r' "$days" "$(date -u -d "@$time" +%H:%M:%S)"
        sleep 0.1
    done
}

stopwatch() {
    start=$(date +%s)
    while true; do
        days="$(($(( $(date +%s) - $start )) / 86400))"
        time="$(( $(date +%s) - $start ))"
        printf '%s day(s) and %s\r' "$days" "$(date -u -d "@$time" +%H:%M:%S)"
        sleep 0.1
    done
}

# the file lognumber stores the number of times I've logged in to bash/zsh, and everytime this bashrc is run, that number is updated.
[ ! -e ~/lognumber ] && echo 0 > ~/lognumber
expr $(cat ~/lognumber) + 1 > ~/lognumber
echo -e "$(cat ~/lognumber) logins so far\n"

cal

# ----------------------------------------
# SOURCING PLUGINS AND SHELL CONFIGURATION
# ----------------------------------------

# why is there no history logging by default lol
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

bindkey -v
bindkey '^R' history-incremental-search-backward

zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
autoload -Uz compinit && compinit

setopt auto_cd
setopt interactive_comments

source ~/zplugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
