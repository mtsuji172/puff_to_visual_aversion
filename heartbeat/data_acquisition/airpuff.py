#!/usr/bin/python

import serial

class AirPuff(object):
    def __init__(self):
        self.arduino = serial.Serial('COM12', 115200)

    def switch(self, onoff):
        if onoff=='ON':
            self.arduino.write(str.encode('0'))
            self.currentPuffState = 'ON'
        else:
            self.arduino.write(str.encode('1'))
            self.currentPuffState = 'OFF'
