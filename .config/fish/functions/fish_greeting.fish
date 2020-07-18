# Defined in /tmp/fish.3i932I/fish_greeting.fish @ line 2
function fish_greeting
    cat /home/phil/Images/manjaro-banner.logo
    echo
	hello_user.sh --no-logo /home/phil/Data
	echo
	fortune -a
	echo
end
