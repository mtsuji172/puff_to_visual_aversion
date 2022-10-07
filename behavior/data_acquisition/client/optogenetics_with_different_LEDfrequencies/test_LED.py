#!/usr/bin/python

import ard
import time

print('warming arduino...')
arduino = ard.Ard()
time.sleep(3) # let arduino warm

print('starting session...')
arduino.switchLED("ON")
time.sleep(10)
arduino.switchLED("OFF")
