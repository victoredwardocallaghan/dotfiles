#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ -f ~/.bash_alias ]] && source ~/.bash_alias

if [ "$TERM" = "linux" ]; then
    _SEDCMD='s/.*\*color\([0-9]\{1,\}\).*#\([0-9a-fA-F]\{6\}\).*/\1 \2/p'
    for i in $(sed -n "$_SEDCMD" $HOME/.Xresources | \
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

# Git PS1 Prompt
source ~/.git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWCOLORHINTS=true
GIT_PS1_DESCRIBE_STYLE="branch"

#export PATH="/usr/lib/cw:$PATH"
#export CW_COLORIZE=purple:green
#export NOCOLOR_PIPE=1

## Andorid kernel.
export ARCH=arm
export CROSS_COMPILE=/home/edward/Work/Mike/ToolChain/arm-eabi-4.7/bin/arm-eabi-

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

export VISUAL=vim
export EDITOR=vim

export PATH=$PATH:~/.cabal/bin:/usr/local/android-sdk-linux/platform-tools

export HISTCONTROL=erasedups
export HISTSIZE=500
export HISTIGNORE=ls:'ls -l':fg


#PS1="\[\e[01;31m\]┌─[\[\e[01;35m\u\e[01;31m\]]──[\[\e[00;37m\]${HOSTNAME%%.*}\[\e[01;32m\]]:\w$\[\e[01;31m\]\n\[\e[01;31m\]└──\[\e[01;36m\]>>\[\e[0m\]"

# Debug PS1 prompt, check color has been reset.
#trap 'echo -ne "\e[0m"' DEBUG
#set -x

PS1='\[\e[0;37m\]┌─[$(battery_status)\[\e[0;37m\]]\j[$(user_color)\u\[\e[0;37m\]][\[\e[0;96m\]\h\[\e[0;37m\]][\[\e[0;32m\]\w\[\e[0;37m\]][\[\e[03;35m\]\W$(__git_ps1 " (%s)")\[\e[0;37m\]] \n\[\e[0;37m\]└──╼ \[\e[0m\]'
PS2="╾──╼"


#Functions

user_color () {

if [ `id -u` -eq "0" ]; then
	root="${BRed}"
else
	root="${BPurple}"
fi

echo -e "${root}"
}

#Check battery levels
battery_status()
{
#BATTERY=/proc/acpi/battery/BAT0
BATTERY=/sys/class/power_supply/BAT0

#REM_CAP=`grep "^remaining capacity" $BATTERY/state | awk '{ print $3 }'`
#FULL_CAP=`grep "^last full capacity" $BATTERY/info | awk '{ print $4 }'`
#BATSTATE=`grep "^charging state" $BATTERY/state | awk '{ print $3 }'`

REM_CAP=`cat $BATTERY/energy_now`
FULL_CAP=`cat $BATTERY/energy_full`
BATSTATE=`cat $BATTERY/status`

CHARGE=`echo $(( $REM_CAP * 100 / $FULL_CAP ))`

NON='\e[00m'
BLD='\e[01m'
BLKRed='\e[05;91m'

COLOUR="$BLD$BLKRed"
WARNING=""

case "${BATSTATE}" in
   'Charged')
   BATSTT="$BLD=$NON"
   ;;
   'Charging')
   BATSTT="$BLD+$NON"
   ;;
   'Discharging')
   BATSTT="$BLD-$NON"
   ;;
esac

# prevent a charge of more than 100% displaying
if [ "$CHARGE" -gt "99" ]
then
   CHARGE=100
fi

if [ "$CHARGE" -lt "5" ]
then
	WARNING="${BLKRed}( BATTERY WARNING! ) "
fi

if [ "$CHARGE" -gt "15" ]
then
   COLOUR="$Yellow"
fi

if [ "$CHARGE" -gt "30" ]
then
   COLOUR="$Green"
fi

echo -e "${WARNING}${BATSTT}${COLOUR}${CHARGE}%${NON}"
}

#Create archive

compress () {
    if [ -n "$1" ] ; then
        FILE=$1
        case $FILE in
        *.tar)      shift && tar cf $FILE $* ;;
        *.tar.bz2)  shift && tar cjf $FILE $* ;;
        *.tar.gz)   shift && tar czf $FILE $* ;;
        *.tgz)      shift && tar czf $FILE $* ;;
        *.zip)      shift && zip $FILE $* ;;
        *.rar)      shift && rar $FILE $* ;;
        esac
    else
        echo "usage: compress <archive.tar.gz> <archive> <files>"
    fi
}

#Unpack archive

unpack() {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)  tar xjf $1    ;;
            *.tbz2)     tar xjf $1    ;;
            *.tar.gz)   tar xzf $1    ;;
            *.tgz)      tar xzf $1    ;;
            *.bz2)      bunzip2 $1    ;;
            *.rar)      unrar x $1    ;;
            *.gz)       gunzip $1     ;;
            *.tar)      tar xf $1     ;;
            *.zip)      unzip $1      ;;
            *.Z)        uncompress $1 ;;
            *.7z)       7z x $1       ;;
            *) echo -e ${YELLOW}"'$1' cannot be unpacked"${RESET} ;;
        esac
    else
        echo -e ${YELLOW}"'$1' is an invalid file"${RESET}
    fi
}


#Generate random password

genpasswd() {
        local l=$1
        [ "$l" == "" ] && l=16
        tr -dc A-Za-z0-9_ < /dev/urandom | head -c ${l} | xargs
}
