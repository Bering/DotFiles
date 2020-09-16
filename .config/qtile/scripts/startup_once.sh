#!/bin/bash

function run {
  if ! pgrep $1; then
    $@&
  fi
}

feh --no-fehbg --bg-scale '/home/phil/Images/camo tech manjaro.jpg'
run picom &
run deadd-notification-center &
run nm-applet &
run pasystray &
run blueman-tray &
run pamac-tray &
run cbatticon &
run redshift-gtk &
run udiskie -a -s &
run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
