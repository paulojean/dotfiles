#!/usr/bin/env bash

i3-msg -t command unmark prev

i3-msg -t command mark prev

i3-msg -t command workspace "$1"
