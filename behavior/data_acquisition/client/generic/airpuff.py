#!/usr/bin/python

import serial

class AirPuff(object):
    def __init__(self):
        self.arduino = serial.Serial('/dev/ttyACM1', 15200)
        self.currentPuffState = 'OFF'
        self.currentLedState = 'OFF'

    def switch(self, onoff):
        if onoff=='ON':
            self.arduino.write('0')
            self.currentPuffState = 'ON'
        else:
            self.arduino.write('1')
            self.currentPuffState = 'OFF'
    
    def switchLED(self, onoff):
        if onoff=='ON':
            self.arduino.write('2')
            self.currentLedState = 'ON'
        else:
            self.arduino.write('3')
            self.currentLedState = 'OFF'
