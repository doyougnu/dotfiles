#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 3; done

# Launch bar1 and bar2
polybar top --config=/home/doyougnu/.config/polybar/config.ini

echo "Bars launched..."
