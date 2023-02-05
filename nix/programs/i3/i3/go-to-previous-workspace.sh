#!/usr/bin/env bash

i3-msg -t command mark prev-temp
i3-msg -t command [con_mark="prev"] focus
i3-msg -t command unmark prev
i3-msg -t command [con_mark="prev-temp"] mark prev
