#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ -f ~/.bash_alias ]] && source ~/.bash_alias
[[ -f ~/.bash_functions ]] && source ~/.bash_functions

if [ "$TERM" = "linux" ]; then
    _SEDCMD='s/.*\*color\([0-9]\{1,\}\).*#\([0-9a-fA-F]\{6\}\).*/\1 \2/p'
    for i in $(sed -n "$_SEDCMD" $HOME/.Xresources.colours | \
               awk '$1 < 16 {printf "\\e]P%X%s", $1, $2}'); do
        echo -en "$i"
    done
    clear
fi

if [ -f ~/.dir_colors ]; then
    eval `dircolors ~/.dir_colors`
fi

# Bash Completion
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

unset SSH_ASKPASS

# bash options ------------------------------------
shopt -s autocd             # change to named directory
shopt -s cdable_vars        # if cd arg is not valid, assumes its a var defining a dir
shopt -s cdspell            # autocorrects cd misspellings
shopt -s checkwinsize       # update the value of LINES and COLUMNS after each command if altered
shopt -s cmdhist            # save multi-line commands in history as single line
shopt -s dotglob            # include dotfiles in pathname expansion
shopt -s expand_aliases     # expand aliases
shopt -s extglob            # enable extended pattern-matching features
shopt -s histappend         # append to (not overwrite) the history file
shopt -s hostcomplete       # attempt hostname expansion when @ is at the beginning of a word
shopt -s nocaseglob         # pathname expansion will be treated as case-insensitive
shopt -s checkjobs          # bash lists the status of any stopped and running jobs before exiting an interactive shell.

# Git PS1 Prompt
source ~/.git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWCOLORHINTS=true
GIT_PS1_DESCRIBE_STYLE="branch"

# Fix for remote systems that don't have
# the rxvt-unicode-256color termcap entry.
#case "$TERM" in
#    rxvt-unicode-256color)
#        TERM=rxvt-unicode
#        ;;
#esac

## Andorid kernel.
export ARCH=arm
#export CROSS_COMPILE=/home/edward/Work/Mike/ToolChain/arm-eabi-4.7/bin/arm-eabi-
export CROSS_COMPILE=/home/edward/Work/Mike/ToolChain/arm-2011.09/bin/arm-none-linux-gnueabi-
#export CROSS_COMPILE=/usr/bin/arm-none-eabi-

# Reset
Color_Off='\e[0m'       # Text Reset

# Regular Colors
Black='\e[0;30m'        # Black
Red='\e[0;31m'          # Red
Green='\e[0;32m'        # Green
Yellow='\e[0;33m'       # Yellow
Blue='\e[0;34m'         # Blue
Purple='\e[0;35m'       # Purple
Cyan='\e[0;36m'         # Cyan
White='\e[0;37m'        # White

# Bold
BBlack='\e[1;30m'       # Black
BRed='\e[1;31m'         # Red
BGreen='\e[1;32m'       # Green
BYellow='\e[1;33m'      # Yellow
BBlue='\e[1;34m'        # Blue
BPurple='\e[1;35m'      # Purple
BCyan='\e[1;36m'        # Cyan
BWhite='\e[1;37m'       # White

# Underline
UBlack='\e[4;30m'       # Black
URed='\e[4;31m'         # Red
UGreen='\e[4;32m'       # Green
UYellow='\e[4;33m'      # Yellow
UBlue='\e[4;34m'        # Blue
UPurple='\e[4;35m'      # Purple
UCyan='\e[4;36m'        # Cyan
UWhite='\e[4;37m'       # White

# Background
On_Black='\e[40m'       # Black
On_Red='\e[41m'         # Red
On_Green='\e[42m'       # Green
On_Yellow='\e[43m'      # Yellow
On_Blue='\e[44m'        # Blue
On_Purple='\e[45m'      # Purple
On_Cyan='\e[46m'        # Cyan
On_White='\e[47m'       # White

# High Intensity
IBlack='\e[0;90m'       # Black
IRed='\e[0;91m'         # Red
IGreen='\e[0;92m'       # Green
IYellow='\e[0;93m'      # Yellow
IBlue='\e[0;94m'        # Blue
IPurple='\e[0;95m'      # Purple
ICyan='\e[0;96m'        # Cyan
IWhite='\e[0;97m'       # White

# Bold High Intensity
BIBlack='\e[1;90m'      # Black
BIRed='\e[1;91m'        # Red
BIGreen='\e[1;92m'      # Green
BIYellow='\e[1;93m'     # Yellow
BIBlue='\e[1;94m'       # Blue
BIPurple='\e[1;95m'     # Purple
BICyan='\e[1;96m'       # Cyan
BIWhite='\e[1;97m'      # White

# High Intensity backgrounds
On_IBlack='\e[0;100m'   # Black
On_IRed='\e[0;101m'     # Red
On_IGreen='\e[0;102m'   # Green
On_IYellow='\e[0;103m'  # Yellow
On_IBlue='\e[0;104m'    # Blue
On_IPurple='\e[10;95m'  # Purple
On_ICyan='\e[0;106m'    # Cyan
On_IWhite='\e[0;107m'   # White

#
export VISUAL=vim
export EDITOR=vim

# Use ViM Pager as the default system pager instead of less
export PAGER=/usr/bin/vimpager
export SDCV_PAGER=$PAGER
alias less=$PAGER
alias zless=$PAGER
export GREP_COLOR="01;33"

export PATH=$PATH:~/.cabal/bin:/usr/local/android-sdk-linux/platform-tools
export PATH=$PATH:/usr/local/cryptol-academic-1.8.19/bin

# colorgcc package:
export PATH="/usr/lib/colorgcc/bin:$PATH"
#export PATH="/usr/lib/cw:$PATH"
#export CW_COLORIZE=purple:green
#export NOCOLOR_PIPE=1

# Gtk-2.0 theme
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

#---------------------------------------
# History stuff
#---------------------------------------
export HISTCONTROL=erasedups
export HISTSIZE=500
export HISTIGNORE=ls:'ls -l':fg
shopt -s histappend
export PROMPT_COMMAND="history -a; history -n"

#PS1="\[\e[01;31m\]┌─[\[\e[01;35m\u\e[01;31m\]]──[\[\e[00;37m\]${HOSTNAME%%.*}\[\e[01;32m\]]:\w$\[\e[01;31m\]\n\[\e[01;31m\]└──\[\e[01;36m\]>>\[\e[0m\]"

# Debug PS1 prompt, check color has been reset.
#trap 'echo -ne "\e[0m"' DEBUG
#set -x

PS1='\[\e[0;37m\]┌─[$(battery_status)\[\e[0;37m\]]\j[$(user_color)\u\[\e[0;37m\]][\[\e[0;96m\]\h\[\e[0;37m\]][\[\e[0;32m\]\w\[\e[0;37m\]][\[\e[03;35m\]\W$(__git_ps1 " (%s)")\[\e[0;37m\]] \n\[\e[0;37m\]└──╼ \[\e[0m\]'
PS2="╾──╼"

