#!/usr/bin/env bash
# original script created by clusterF modified
# by ManyRoads
#

while true; do

        date '' > /tmp/CurTime.tmp

        sleep 60s
done &

while true; do

        LOCALTIME=$(date '+%a %d/%m/%Y|%H:%M')
        VOL=$(amixer get Master | awk -F'[][]' 'END{ print $4":"$2 }')
        MEM=$(free -h --kilo | awk '/^Mem:/ {print $3 "/" $2}')
        CPU=$(top -bn1 | grep "Cpu(s)" | sed "s/.*, *\([0-9.]*\)%* id.*/\1/" | awk '{print 100 - $1}' )
        TEMP=$(sensors|grep 'Core 0'|awk '{print $3}' )
        #DISK=$(df -Ph | grep "/dev/sda3" | awk {'print $5'})
	WIFI=$(nmcli -f ACTIVE,SIGNAL dev wifi list | awk '$1=="yes" {print $2}')
	BAT=$(cat /sys/class/power_supply/BAT0/capacity)
	BAT_STATUS=$(cat /sys/class/power_supply/BAT0/status)
	xsetroot -name "  $MEM   $CPU%  $TEMP   $BAT% $BAT_STATUS 墳 $VOL  $WIFI%  $LOCALTIME"
        sleep 2s
done &
