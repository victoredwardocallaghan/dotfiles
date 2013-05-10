#
# ~/.bash_functions
#


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
        *.7z)      shift && 7z a $FILE $* ;;
        *.rar)      shift && rar $FILE $* ;;
        esac
    else
        echo "usage: compress <archive.tar.gz> <archive> <files>"
    fi
}

#Extract archive

function extract() {
 local e=0 i c
 for i; do
   if [ -f $i && -r $i ]; then
       c=
       case $i in
         *.tar.bz2) c='tar xjf'    ;;
         *.tar.gz)  c='tar xzf'    ;;
         *.bz2)     c='bunzip2'    ;;
         *.gz)      c='gunzip'     ;;
         *.tar)     c='tar xf'     ;;
         *.tbz2)    c='tar xjf'    ;;
         *.tgz)     c='tar xzf'    ;;
         *.7z)      c='7z x'       ;;
         *.Z)       c='uncompress' ;;
         *.exe)     c='cabextract' ;;
         *.rar)     c='unrar x'    ;;
         *.xz)      c='unxz'       ;;
         *.zip)     c='unzip'      ;;
         *)     echo "$0: cannot extract \`$i': Unrecognized file extension" >&2; e=1 ;;
       esac
       [ $c ] && command $c "$i"
   else
       echo "$0: cannot extract \`$i': File is unreadable" >&2; e=2
   fi
 done
 return $e
}

function docview () {
  if [ -f $1 ] ; then
      case $1 in
          *.pdf)       mupdf $1    ;;
          *.ps)        oowriter $1    ;;
          *.odt)       oowriter $1     ;;
          *.txt)       gvim $1       ;;
          *.doc)       oowriter $1      ;;
          *)           echo "Unknown document type to view, '$1'..." ;;
      esac
  else
      echo "'$1' is not a valid file!"
  fi
}

#Generate random password

genpasswd() {
        local l=$1
        [ "$l" == "" ] && l=16
        tr -dc A-Za-z0-9_ < /dev/urandom | head -c ${l} | xargs
}
# Or second version
function randompassword(){
  cat /dev/urandom | tr -cd '[:graph:]' | head -c ${1:-12}
}


#Git Helpers

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo " *"
}

function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}

# Shell env

function env() {
	exec /usr/bin/env "$@" | grep -v ^LESS_TERMCAP_
} 

# Fetch current song on last.fm
function lastfm_np(){
  curl -s http://www.last.fm/user/$1 | grep -A 1 subjectCell | sed -e 's#<[^>]*>##g' | head -n2 | tail -n1 | sed 's/^[[:space:]]*//g'
}

# Fetch last loved track
function lastfm_lastloved() {
  ITEM="$(curl --silent http://ws.audioscrobbler.com/2.0/user/$1/lovedtracks.rss | head -20 | grep -A 5 '<item>')"
  echo $ITEM | sed -r 's/.*<title>([^<]*).*/\1/'
}

# find a string in a set of files
function fstr(){
  if [ "$#" -gt 2 ]; then
    echo "Usage: fstr \"pattern\" [files] "
    return;
  fi
  SMSO=$(tput smso)
  RMSO=$(tput rmso)
  find . -type f -name "${2:-*}" -print | xargs grep -sin "$1" | \
  sed "s/$1/$SMSO$1$RMSO/gI"
}

# find a file
function ff() { 
	find . -iname '*'$1'*' ; 
}

function lls(){ 
	ls -l "$@"| egrep "^d" ; ls -lXB "$@" 2>&-| egrep -v "^d|total "; 
}

# batchexec
function batchexec(){
  find . -type f -iname '*.'${1}'' -exec ${@:2}  {} \; ;	
}

function zombies() {
  local ZOMBIES=$(ps hr -Nos | awk '$1=="Z" {print $1}' | wc -l)
  if [ ${ZOMBIES} -gt 0 ]; then
    echo ${ZOMBIES}
  fi
}

function calc() {
	echo "$*" | bc 
}

function cs() {
	cd "$1"
	ls
}

function psgrep() {
	ps aux | grep "$1" | grep -v "grep"
}

function dirselect() {
	local possibleDirs
	declare -a possibleDirs=("${!1}")
	local i
	local dir
	local dirs=()
	let i=0
	for dir in "${possibleDirs[@]}"; do
		if [ -d "$dir" ] && [ -x "$dir" ]; then
			echo -e "$bldwht[$bldred${i}$bldwht]$txtrst $dir"
			dirs[$i]="$dir"
			let i++
		fi
	done
	if [ ${#dirs[*]} -eq 1 ]; then
		cs "${dirs[0]}"
	elif [ ${#dirs[*]} -gt 1 ]; then
		echo -n -e "$bldred>>$txtrst "
		local sel
		read sel
		if [ ! -z "${dirs[$sel]}" ]; then
			cs "${dirs[$sel]}"
		fi
	fi
}

function ldir() {
	if [ -z "$1" ]; then
		return
	fi
	local lresult
	mapfile -t lresult < <(locate -b -e -q -- "$1"|grep -Pv '\/\.'|sort)
	dirselect lresult[@]
}

# bookmark directory
function bmdir() {
	local dir
	if [ -z "$1" ]; then
		dir="$PWD"
	else
		dir="$1"
	fi
	echo "$dir">>~/.bmdir
}

#jump to bookmark
function jbm() {
	if [ ! -f ~/.bmdir ]; then
		return
	fi
	local lresult
	mapfile -t lresult < <(grep -e "$1"< ~/.bmdir)
	dirselect lresult[@]
}

# http://pastebin.com/api.php
function pastebin() {
	curl -s --data-urlencode paste_code@- -d paste_name="$USER" -d paste_expire_date=N http://pastebin.com/api_public.php|tail -n 1
	echo
}


function copySshKey(){
	cat ~/.ssh/id_rsa.pub | ssh $1 'cat >> .ssh/authorized_keys'
}

function extDisplay(){
	if ! xrandr | grep VGA1 | grep disconnected  &gt;/dev/null ; then
		xrandr --output LVDS1 --mode 1024x768 --output VGA1 --mode 1024x786 --above LVDS1
	else
		xrandr --auto
	fi
	}

function shrinkurl(){
	curl https://www.googleapis.com/urlshortener/v1/url -H 'Content-Type: application/json' -d '{"longUrl": "${1}"}'
}

function clock(){
	while true;do clear;echo "===========";date +"%r";echo "===========";sleep 1;done
}

function whatthecommit(){
	curl -s http://whatthecommit.com/index.txt
}

psg() { 
	ps auxw | grep -v "grep" | grep -E "(^USER.*COMMAND$|$@)"; 
}
