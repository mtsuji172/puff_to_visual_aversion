#!/bin/bash
opticalsensor=$(xinput list | grep 'HID-compliant Mouse' | awk '{print $7}' | awk -F\id= '{ print $2 }')
xinput set-prop $opticalsensor 'Device Enabled' $1
