function config --wraps=git --description 'Wrapper for git for /home/phil/DotFiles'
    git --git-dir=/home/phil/DotFiles --work-tree=/home/phil $argv
end
