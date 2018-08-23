# Setting PATH for Python 3.6
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
export PATH

# Juan Casse for virtualenvwrapper
export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/Library/Frameworks/Python.framework/Versions/3.6/bin/python3
export VIRTUALENVWRAPPER_VIRTUALENV=/Library/Frameworks/Python.framework/Versions/3.6/bin/virtualenv
source /Library/Frameworks/Python.framework/Versions/3.6/bin/virtualenvwrapper.sh

# Juan Casse for MongoDB
export PATH="/Users/jcasse/mongodb/bin:$PATH"

# added by Anaconda3 5.0.0 installer
export PATH="/Users/jcasse/anaconda3/bin:$PATH"

################################################################################
## GPG agent support.

# Is there an agent running?
if [[ -f "${HOME}/.gnupg/.gpg-agent-info" ]]
then
    # If so, insert the appropriate environment variables into this shell.
    source "${HOME}/.gnupg/.gpg-agent-info"
fi
export GPG_TTY=$(tty)

###############################################################################
## Prevent history from being lost just because we have multiple shells open
## (which is a situation that happens for me all the time.)
##
## This trick came from BashFAQ #88: http://mywiki.wooledge.org/BashFAQ/088

# Make the history file span several years.  (It seems that unsetting
# $HISTFILESIZE altogether is no longer recommended practice.)
export HISTFILESIZE=100000

# The number of commands to copy into ~/.bash_history from the history of an
# interactive shell when that shell exits.  This is also the number of lines
# of history that are read from the history file into memory when an
# interactive shell first opens.
export HISTSIZE=100000

# Before displaying every prompt, commit the current history.
if [[ $PROMPT_COMMAND == "" ]]; then
    export PROMPT_COMMAND="history -a"
else
    export PROMPT_COMMAND="$PROMPT_COMMAND ; history -a"
fi

# Append the history to the HISTFILE when exiting instead of overwriting it.
shopt -s histappend

# Make the "history" command print timestamps in RFC 3339 format
# (2008-05-19T18:39:00Z).
export HISTTIMEFORMAT="%Y-%m-%dT%H:%M:%S%z "

# If we repeat commands, only commit one to history.  Also, delete ALL
# previous occurrences of any command we enter!  Hopefully this will keep the
# size low for some very commom commands (like "dir").  That doesn't absolve
# us of the need to archive, though.
export HISTCONTROL="ignoredups:erasedups"

# Vim-like editing in command line
# By default, this is set to Emacs: set -o emacs
set -o vi

# added by Juan Casse
# Needed by Emacs
export PATH="/Library/TeX/texbin:$PATH"

# Execute the following to update the Finder PATH
#launchctl setenv PATH $PATH
#killall Dock
