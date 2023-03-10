#!/usr/bin/python

import serial

class Ard(object):
    def __init__(self):
        self.puffboard = '/dev/ttyACM1'
        self.LEDboard = '/dev/ttyACM0'
        self.arduino_puff = serial.Serial(self.puffboard, 15200)
        self.arduino_LED = serial.Serial(self.LEDboard, 19200)
        self.puffstate = 'OFF'
        self.LEDstate = 'OFF'

    def switchPuff(self, onoff):
        if onoff=='ON':
            self.arduino_puff.write('0')
            self.puffstate = 'ON'
        else:
            self.arduino_puff.write('1')
            self.puffstate = 'OFF'
    
    def switchLED(self, onoff):
        if onoff=='ON':
            self.arduino_LED.write('0')
            self.LEDstate = 'ON'
        else:
            self.arduino_LED.write('1')
            self.LEDstate = 'OFF'
