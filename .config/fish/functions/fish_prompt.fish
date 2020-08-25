function fish_prompt

    # This prompt shows:
    # - green lines if the last return command is OK, red otherwise
    # - your user name, in red if root or yellow otherwise
    # - your hostname, in cyan if ssh or blue otherwise
    # - the current path (with prompt_pwd)
    # - date +%X
    # - the current virtual environment, if any
    # - the current git status, if any, with fish_git_prompt
    # - the current battery state, if any, and if your power cable is unplugged, and if you have "acpi"
    # - current background jobs, if any

    # It goes from:
    # ┬─[nim@Hattori:~]
    # ╰─>$ echo here

    # To:
    # ┬─[nim@Hattori:~/w/dashboard]─[11:37:14]─[V:django20]─[G:master↑1|●1✚1…1]─[B:85%, 05:41:42 remaining]
    # │ 2	15054	0%	arrêtée	sleep 100000
    # │ 1	15048	0%	arrêtée	sleep 100000
    # ╰─>$ echo there

    # Modifications by Bering:
    # - Changed a bunch of colors
    # - White T: before the time, for consistency with other blocks
    # - Show $status if it is not 0

    # saving $status so I can look it up later because it will be overwritten while running commands in this function
    set -l laststatus $status

    set -l retc red
    test $status = 0; and set retc brgreen

    set -q __fish_git_prompt_showupstream
    or set -g __fish_git_prompt_showupstream auto

    function _nim_prompt_wrapper
        set retc $argv[1]
        set field_name $argv[2]
        set field_value $argv[3]

        set_color normal
        set_color $retc
        echo -n '─'
        set_color -o yellow
        echo -n '['
        set_color normal
        test -n $field_name
        and echo -n $field_name ""
        set_color $retc
        echo -n $field_value
        set_color -o yellow
        echo -n ']'
    end

    set_color $retc
    echo -n '┬─'

    # username@host:pwd
    set_color -o yellow
    echo -n [
    if test "$USER" = root -o "$USER" = toor
        set_color -o red
    else
        set_color -o brgreen
    end
    echo -n $USER
    set_color -o white
    echo -n @
    if [ -z "$SSH_CLIENT" ]
        set_color -o blue
    else
        set_color -o cyan
    end
    echo -n (prompt_hostname)
    set_color -o white
    echo -n :(prompt_pwd)
    set_color -o yellow
    echo -n ']'

    # Date
    #_nim_prompt_wrapper $retc  (date +%X)

    # Virtual Environment
    set -q VIRTUAL_ENV_DISABLE_PROMPT
    or set -g VIRTUAL_ENV_DISABLE_PROMPT true
    set -q VIRTUAL_ENV
    and _nim_prompt_wrapper $retc V (basename "$VIRTUAL_ENV")

    # git
    set prompt_git (fish_git_prompt | string trim -c ' ()')
    test -n "$prompt_git"
    and _nim_prompt_wrapper $retc  $prompt_git

    # Battery status
    type -q acpi
    and test (acpi -a 2> /dev/null | string match -r off)
    and _nim_prompt_wrapper $retc  (acpi -b | cut -d' ' -f 4-)

    # New line
    echo

    # Background jobs
    set_color normal
    for job in (jobs)
        set_color $retc
        echo -n '│ '
        set_color brown
        echo $job
    end

    # Status, if not 0
    if [ $laststatus -ne 0 ]
        set_color red
        echo -n '│ '
        set_color yellow
        echo -n '['
        set_color brwhite
        echo -n 'S:'
        set_color $retc
        echo -n $laststatus
        set_color yellow
        echo -n ']'
        echo
    end

    # Last line of prompt
    set_color normal
    set_color $retc
    echo -n '╰─>'
    set_color -o red
    echo -n '$ '
    set_color normal
end
