function fish_right_prompt
    set -l retc red
    test $status = 0; and set retc green
    
    set_color $retc
    echo -e '\e[A'
    echo -n '─['
    date +%X
    echo -n ']'
    echo -e '\e[B'
end
