#!/usr/bin/env bash

# Terminate already running bar instances
pkill polybar || true
# If all your bars have ipc enabled, you can also use
# polybar-msg cmd quit

echo "---" | tee -a /tmp/polybar1.log

# Launch polybar in all monitors
for m in $(polybar --list-monitors | cut -d":" -f1); do
  echo "Launch polybar on monitor < $m >"
  MONITOR=$m polybar bar1 >>/tmp/polybar1.log 2>&1 &
done

echo "Bars launched..."
