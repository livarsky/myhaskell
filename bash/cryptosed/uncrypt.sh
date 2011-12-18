#!/bin/bash

dest=$1

for i in $(seq 0 9999)
do
    pass=$(printf "%04d" $i)
    status="NO"
    if 7z e -yp"$pass" "$dest" >/dev/null 2>&1
    then
	status="YES"	
	echo "Current pass is $pass: status: $status"
	exit 0
    fi
    echo "Current pass is $pass: status: $status"	
done

echo "cant find pass"
exit 1

