# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:/$HOME/.local/bin:$PATH

# Path to your oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh/

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="nanotech"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)


# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

#upgrade alias
alias updateSys="sudo pacman -Syyu && stack update"

# Alias to auto connect to os2 Operating Systems 2 server
alias connOS2="ssh -J youngjef@flip.engr.oregonstate.edu youngjef@os2.engr.oregonstate.edu"

# function to pull a file to os2 server
pullFile () {
	case "$#" in 
		1)  echo "Pulling $1 from server to home dir"
			$(scp -oProxyJump=youngjef@flip.engr.oregonstate.edu youngjef@os2.engr.oregonstate.edu:~/$1 ~/)
			echo "All Done!"
			return 0
			;;

		2)  echo "Pulling $1 from server to $2"
			$(scp -oProxyJump=youngjef@flip.engr.oregonstate.edu youngjef@os2.engr.oregonstate.edu:~/$1 $2)
			echo "All Done!"
			return 0
			;;

		*)	echo "Usage: $0 file_name [destination]"
			return 1
			;;
	esac
}

# function to pull a file to os2 server
pushFile () {
	case "$#" in 
		1)  echo "Pushing $1 to server home dir"
			$(scp -oProxyJump=youngjef@flip.engr.oregonstate.edu $1 youngjef@os2.engr.oregonstate.edu:~/)
			echo "All Done!"
			return 0
			;;

		2)  echo "Pushing $1 to server to $2"
			$(scp -oProxyJump=youngjef@flip.engr.oregonstate.edu $1 youngjef@os2.engr.oregonstate.edu:~/$2)
			echo "All Done!"
			return 0
			;;

		*)	echo "Usage: $0 file_name [destination]"
			return 1
			;;
	esac
}

#add rust to path
export PATH="${PATH}:$HOME/.cargo/env"

# sourcing for virtualenvwrapper, python 3
source /usr/bin/virtualenvwrapper.sh

# Environment var for postgres data directory
export PGDATA="/var/lib/postgres/data"

#function wrapUp { zle up-history }
#function wrapDown { zle down-history }
## Better history keybinds
#bindkey -M viins '^k' wrapUp
#bindkey -M vicmd '^k' wrapDown
# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

ZSH_CACHE_DIR=$HOME/.cache/oh-my-zsh
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

source $ZSH/oh-my-zsh.sh
