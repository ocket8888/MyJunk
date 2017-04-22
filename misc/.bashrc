# To the extent possible under law, the author(s) have dedicated all 
# copyright and related and neighboring rights to this software to the 
# public domain worldwide. This software is distributed without any warranty. 
# You should have received a copy of the CC0 Public Domain Dedication along 
# with this software. 
# If not, see <http://creativecommons.org/publicdomain/zero/1.0/>. 

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return


# Default to human readable figures
alias df='df -h'
alias du='du -h'
#
# Misc :)
# alias less='less -r'                          # raw control characters
# alias whence='type -a'                        # where, of a sort
alias grep='grep --color'                     # show differences in colour
alias egrep='egrep --color=auto'              # show differences in colour
alias fgrep='fgrep --color=auto'              # show differences in colour
#
# Some shortcuts for different directory listings
alias ls='ls -hF --color=tty'                 # classify files in colour
alias dir='ls --color=auto --format=vertical'
# alias vdir='ls --color=auto --format=long'
alias ll='ls -l'                              # long list
alias la='ls -A'                              # all but . and ..
alias l='ls -CF'                              #
alias lla='ls -lA'

#some shortcuts for clearing the screen and showing dir contents
alias cls='clear && ls'
alias cll='clear && ll'
alias cla='clear && la'
alias clla='clear && lla'

#Incode stuff
export FDTN=/cygdrive/c/Incode10/Foundation
export IX=/cygdrive/c/Incode10/PublicApi/ExpressWeb
export DISPSERV='/cygdrive/c/Incode10/bin/Debug/DispatchService.exe'
export SECSERV='/cygdrive/c/Incode10/bin/SecurityService/Debug/SecurityService.exe'
export EXPRSERV='/cygdrive/c/Incode10/bin/ExpressService/Debug/ExpressService.exe'
export CRM='/cygdrive/c/Incode10/CRM'

alias msbuild='/cygdrive/c/Program\ Files\ \(x86\)/MSBuild/14\.0/Bin/MSBuild\.exe /m'
alias make='time make -j --warn-undefined-variables'
alias buildall='/cygdrive/c/Incode10/BuildAll.bat'
alias buildapi='pushd $FDTN && msbuild API.sln && popd'
alias runapi='cygstart $DISPSERV && cygstart $SECSERV && cygstart $EXPRSERV'
alias api='buildapi && runapi'
alias dispatch='cygstart $DISPSERV'
alias gulp='C:/Incode10/PublicApi/ExpressWeb/node_modules/gulp/bin/gulp.js --max_old_space_size=1500'

function buildix {
	if [[ -f $IX/makefile || -f $IX/Makefile ]]; then
		make -I $IX;
	else
		pushd $IX;
		gulp;
		popd;
	fi
}

alias begin='buildall && dispatch; buildix;'

cd /cygdrive/c/Incode10
/cygdrive/c/Program\ Files\ \(x86\)/Microsoft\ Visual\ Studio\ 14.0/VC/vcvarsall.bat x86