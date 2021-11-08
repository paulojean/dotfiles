#!/usr/bin/env bash

if [[ "$(ps aux | grep [e]ww)" == "0" ]]; then
  eww reload
else
  eww daemon
  eww open-many "bar"
fi
