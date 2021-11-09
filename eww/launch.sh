#!/usr/bin/env bash

if [[ "$(ps aux | grep [e]ww)" == "0" ]]; then
  eww reload
else
  eww daemon
  while [[ "$(eww windows)" != *"*bar"* ]]
  do
    eww open "bar"
  done
fi
