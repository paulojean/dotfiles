#!/usr/bin/env bash

i3-msg -t command move container to workspace "$1"

eww update workspaces="$("$HOME/.config/eww/scripts/workspaces")"
