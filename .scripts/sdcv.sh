#!/bin/bash
# define - command line dictionary

# text color variables
bldred='\e[1;31m'     # red    - bold
bldwht='\e[1;37m'     # white  - bold
txtrst='\e[0m'        # text reset

sedbldwht='\c[[1;37m' # white  - bold
sedtxtrst='\c[[0m'    # text reset

# display usage if full argument isn't given
if [[ -z $1 ]]; then
  echo " define <word-to-lookup> - command line dictionary"
  exit
fi

# suggest possible words if not in dictionary, otherwise define
wordchecknum=$(echo "$1" | aspell -a | sed '1d' | wc -m)
wordcheckprnt=$(echo "$1" | aspell -a | sed '1d' | sed 's/^.*: //')

if [[ $wordchecknum -gt "3" ]]; then
  echo -e "${bldred}*${txtrst} ${bldwht}"$1"${txtrst} is not in the dictionary.  Possible alternatives:"
  echo -e "\n $wordcheckprnt\n"
  exit
fi

# common word choice menu negate
# doesn't work
#choice=$(sdcv $1 | grep "Your choice" &> /dev/null &)
#if [ -n $choice ]; then
#  echo " Too common a word"
#  exit
#fi

sdcv $1 | \
# lookup, delete extrenous first lines, delete last empty line
sed '1,3d' | sed '/^*$/d' | \
# Print more obvious word type
sed "h; :b; \$b ; N; /^${1}\n     n/ {h;x;s// ${sedbldwht}Noun${sedtxtrst}\n/; bb}; \$b ; P; D" | \
sed "h; :b; \$b ; N; /^${1}\n     v/ {h;x;s// ${sedbldwht}Verb${sedtxtrst}\n/; bb}; \$b ; P; D" | \
sed "h; :b; \$b ; N; /^${1}\n     adv/ {h;x;s// ${sedbldwht}Adverb${sedtxtrst}\n/; bb}; \$b ; P; D" | \
sed "h; :b; \$b ; N; /^${1}\n     adj/ {h;x;s// ${sedbldwht}Adjective${sedtxtrst}\n/; bb}; \$b ; P; D" | \
# Reformat to left (ok, there's gotta be a better way to do this)
sed 's/^     / /g' | \
sed 's/^      /    /g' | \
sed 's/^      /    /g' | \
sed 's/^     /    /g' | \
# Rename single entry
sed 's/^ : / 1: /g' | \
# Pretty colors
sed "s/${1}/${sedbldwht}${1}${sedtxtrst}/g"
