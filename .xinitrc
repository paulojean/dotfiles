#vi: ft = bash

# setxkbmap us intl
# setxkbmap us -variant intl

# xmodmap ~/.xmodmap

# setxkbmap -option 'caps:ctrl_modifier'
xcape -e 'Caps_Lock=Escape'
xinput --set-prop "SynPS/2 Synaptics TouchPad" "libinput Accel Speed" 1.0
# sudo libinput-gestures &
unclutter &

systemctl --user import-environment
systemctl --user start graphical-session.target
systemctl --user start dbus

compton \
  --inactive-opacity 0.9 \
  --opacity-rule "100:class_g = 'Rofi'" \
  --daemon

xrandr --auto

exec bspwm
