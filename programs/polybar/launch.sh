#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar --config=~/.config/polybar/config

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 3; done

# Launch bar1 and bar2
polybar top

echo "Bars launched..."
