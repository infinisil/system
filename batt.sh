#!/usr/bin/env bash

acpiout=$(acpi -b)

battstat=$(echo $acpiout | awk '{print $3}')
battstat=${battstat%?}
charge=$(echo $acpiout | awk '{print $4}' | grep -oP '\d+(?=%)')

case $battstat in
Full)
	;;
Discharging)
	postfix="(-$(date -u -d $(acpi -b | awk '{print $5}') +"%Hh%M"))"
	;;
Charging)
	postfix="(+$(date -u -d $(acpi -b | awk '{print $5}') +"%Hh%M"))"
	;;
*)
	;;
esac


if (($charge <= 12)); then
	icon="<fc=#CE3E25>$charge% </fc>"
elif (($charge <= 37)); then
	icon="<fc=#DB721C>$charge% </fc>"
elif (($charge <= 62)); then
	icon="<fc=#CC9B20>$charge% </fc>"
elif (($charge <= 87)); then
	icon="<fc=#AFAA13>$charge% </fc>"
else
	icon="<fc=#5CBA1A>$charge% </fc>"
fi

echo "$icon $postfix"
