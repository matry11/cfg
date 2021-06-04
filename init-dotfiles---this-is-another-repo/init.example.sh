#!/usr/bin/env bash

# Colors as per: http://www.tldp.org/LDP/abs/html/colorizing.html

echoerrcolor() {
	if (( $colors )); then
		case $1 in
			none)
				str="\e[0;37m"
				;;
			green)
				str="\e[0;32m"
				;;
			red)
				str="\e[0;31m"
				;;
			blue)
				str="\e[1;34m"
				;;
			darkcyan)
				str="\e[0;36m"
				;;
			darkgreen)
				str="\e[1;32m"
				;;
			darkred)
				str="\e[1;31m"
				;;
			magenta)
				str="\e[0;35m"
				;;
			darkmagenta)
				str="\e[1;35m"
				;;

		esac
		echo -ne $str >&2;
	fi
}

echoerrnocolor() {
	if (( $colors )); then
		echo -ne "\e[0m" >&2;
	fi
}

echoerr() { 
	if [ $# -gt 1 ]; then
		color=$1
		shift
		echoerrcolor $color
	fi
	echo "$@" >&2;
	if [ $color ]; then
		echoerrnocolor
	fi
}

printferr() { printf "$@" >&2; }

$(which git >& /dev/null)

if [ $? -eq 1 ]; then
	echoerr red "Git not found! Confirm it is indeed installed and reachable."
	exit;
fi

appendshell() {
	case "$1" in
		start)
			add='echo "Setting up Dotbot. Please do not ^C." >&2;'
			;;
		mkprefix)
			add="mkdir -p $2; cd $2;"
			;;
		gitinit)
			add='git init;'
			;;
		gitaddsub)
			add='git submodule add https://github.com/anishathalye/dotbot;'
			;;
		gitignoredirty)
			add='git config -f .gitmodules submodule.dotbot.ignore dirty;'
			;;
		gitinstallinstall)
			add='cp dotbot/tools/git-submodule/install .;'
			;;
		ensureparentdirs)
			add="mkdir -p $2; rmdir $2;"
			;;
		mv)
			add="mv $2 $3;"
			;;
		echoconfig)
			add='echo -e "'$2'" >> '$3';'
			;;
		runinstaller)
			add='./install;'
			;;
		gitsetname)
			if (( $3 )); then
				global=' --global '
			else
				global=' '
			fi
			add='git config'$global'user.name "'$2'";'
			;;
		gitsetemail)
			if (( $3 )); then
				global=' --global '
			else
				global=' '
			fi
			add='git config'$global'user.email "'$2'";'
			;;
		gitinitialcommit)
			add='git add -A; git commit -m "Initial commit";'
			;;

	esac
	setupshell=$setupshell' '$add
}

testmode=0;
verboseconf=0;
dumpconf=0;
preview=1;
colors=0;


paths=('~/.profile'
	'~/.bashrc'
	'~/.bashrc.orig'
	'~/.bash_logout'
	'~/.emacs.d'
	'~/.gitconfig'
	'~/.ssh/config'
	'~/bin'
	'~/.Xmodmap'
	'~/.Xresources'
	'~/.Xdefaults'
	'~/.xinitrc'
	'~/.xsessionrc')

setupshell=''
dotclean=''
dotlink=''
dotshell=''
installerrun=1;

appendshell start
prefix="~/.cfg"
prefixfull="${prefix/\~/${HOME}}"
appendshell mkprefix $prefix
appendshell gitinit
appendshell gitaddsub
appendshell gitignoredirty
appendshell gitinstallinstall
echoerr green "clean: ['~']"
echoerr darkgreen "option set!"
dotclean="- clean: ['~']"

declare -a linksection;
declare -i i;
for item in ${paths[*]}
do
	fullname="${item/\~/$HOME}"
	if [ -h $fullname ]; then
		continue;
	fi
	if [ -f $fullname ] || [ -d $fullname ]; then
		while true; do
			linksection[$i]=$item;
			i=$i+1
			;;
		done
	fi
done
