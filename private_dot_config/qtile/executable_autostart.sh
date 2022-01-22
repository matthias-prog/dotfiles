#! /bin/bash
picom --config /home/matze/.config/picom/picom.conf &
feh --randomize --bg-fill --no-fehbg ~/Pictures/wallpapers-dt/*
alsactl --file ~/.config/asound.state restore
