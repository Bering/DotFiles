
# History
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000


# Autocomplete (compinstall)
zstyle :compinstall filename '/home/phil/.zshrc'
autoload -Uz compinit
compinit


# various flags
unsetopt beep


# keybindings
bindkey -e
bindkey "\e[3~" delete-char
bindkey "\e[H"  beginning-of-line
bindkey "\e[F"  end-of-line
bindkey "\e[1;5D" backward-word
bindkey "\e[1;5C" forward-word


# Aliases
alias ls='lsd --group-dirs first --date +"%Y-%m-%d %H:%M:%S"'
alias config='git --git-dir=/home/phil/DotFiles --work-tree=/home/phil $argv'


# Prompts (adapted from https://dotshare.it/dots/590)
setopt prompt_subst
setopt promptsubst
setopt promptpercent

autoload colors; colors;

function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "—[ %{$fg[yellow]%}${ref#refs/heads/}%{$reset_color%}]"
}

local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"
local user_host='%{$fg[green]%}%n@%m%{$reset_color%}'
local current_dir='%{$fg[blue]%}%~%{$reset_color%}'
local git_branch='$(git_prompt_info)%{$reset_color%}'

ZSH_THEME_GIT_PROMPT_SUFFIX=""
RPROMPT=""
PROMPT="
%B┬─[${user_host}%B]—[${current_dir}%B]${git_branch}%(1j.
%B│ %j background jobs.)%b
%B%(?..%{$fg[red]%})╰─>%(?.. [%?])%{$reset_color%}%b "
