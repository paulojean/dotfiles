#!/usr/bin/env bash

if [[ "$(ps aux | grep [e]ww)" == "0" ]]; then
  eww reload
else
  eww daemon
  while [[ "$(eww windows)" != *"*bar"* ]]
  do
    eww open "bar"
    sleep 1
  done
  eww update workspaces="$("$HOME/.config/eww/scripts/workspaces")"
  eww update bluetoothState="$(bluetooth | awk '{ printf $3 }')"
fi
