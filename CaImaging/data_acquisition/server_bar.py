#!/usr/bin/python2
# python3 somehow results in broken pattern of visual stimulus...

import argparse
import os
import time
import serial
from rgbmatrix import RGBMatrix, RGBMatrixOptions
import sys
import numpy as np
sys.path.append(os.path.abspath(os.path.dirname(__file__) + '/..'))

class Server(object):
    def __init__(self, *args, **kwargs):
        # argparse
        self.parser = argparse.ArgumentParser()
        self.parser.add_argument("-f", "--puff-frame", action="store", help="frame to start puff and viusal stimuli", default=600, type=str)
        self.parser.add_argument("-r", "--led-rows", action="store", help="Display rows. 16 for 16x32, 32 for 32x32. Default: 16", default=16, type=int)
        self.parser.add_argument("--led-cols", action="store", help="Panel columns. Typically 32 or 64. (Default: 32)", default=32, type=int)
        self.parser.add_argument("-c", "--led-chain", action="store", help="Daisy-chained boards. Default: 6.", default=6, type=int)
        self.parser.add_argument("-P", "--led-parallel", action="store", help="For Plus-models or RPi2: parallel chains. 1..3. Default: 1", default=1, type=int)
        self.parser.add_argument("-p", "--led-pwm-bits", action="store", help="Bits used for PWM. Something between 1..11.", default=11, type=int)
        self.parser.add_argument("-b", "--led-brightness", action="store", help="Sets brightness level. Default: 100. Range: 1..100", default=100, type=int)
        self.parser.add_argument("-m", "--led-gpio-mapping", help="Hardware Mapping: regular, adafruit-hat, adafruit-hat-pwm" , choices=['regular', 'adafruit-hat', 'adafruit-hat-pwm'], type=str, default='adafruit-hat-pwm')
        self.parser.add_argument("--led-scan-mode", action="store", help="Progressive or interlaced scan. 0 Progressive, 1 Interlaced (default)", default=1, choices=range(2), type=int)
        self.parser.add_argument("--led-pwm-lsb-nanoseconds", action="store", help="Base time-unit for the on-time in the lowest significant bit in nanoseconds. Default: 80", default=80, type=int)
        self.parser.add_argument("--led-show-refresh", action="store_true", default=False, help="Shows the current refresh rate of the LED panel")
        self.parser.add_argument("--led-slowdown-gpio", action="store", help="Slow down writing to GPIO. Range: 1..100. Default: 1", choices=range(3), type=int)
        self.parser.add_argument("--led-no-hardware-pulse", action="store", help="Don't use hardware pin-pulse generation")
        self.parser.add_argument("--led-rgb-sequence", action="store", help="Switch if your matrix has led colors swapped. Default: RGB", default="RGB", type=str)
        self.parser.add_argument("--led-row-addr-type", action="store", help="0 = default; 1=AB-addressed panels", default=0, type=int, choices=[0,1])
        self.parser.add_argument("--led-multiplexing", action="store", help="Multiplexing type: 0=direct; 1=strip; 2=checker; 3=spiral (Default: 0)", default=0, type=int, choices=[0,1,2,3])
        self.args = self.parser.parse_args()
        
        # recording params
        self.t_end = 10
        self.width = 8
        self.ymin = 0
        self.ymax = 31
        self.xmin_start = 17
        self.gain = 5.4

        # define screen params
        self.xdimTot = self.args.led_chain * self.args.led_rows
        
        # define colors00
        self.bgcolor = {"R":0, "G":0, "B":5}
        self.objectcolor = {"R":0, "G":0, "B":0}

        # establish connection with arduino
        self.ser = serial.Serial('/dev/ttyACM0', 15000)
        
        # execute self.process
        self.process()
    
    def usleep(self, value):
        time.sleep(value / 1000000.0)

    def process(self):
        options = RGBMatrixOptions()

        if self.args.led_gpio_mapping != None:
          options.hardware_mapping = self.args.led_gpio_mapping
        options.rows = self.args.led_rows
        options.cols = self.args.led_cols
        options.chain_length = self.args.led_chain
        options.parallel = self.args.led_parallel
        options.row_address_type = self.args.led_row_addr_type
        options.multiplexing = self.args.led_multiplexing
        options.pwm_bits = self.args.led_pwm_bits
        options.brightness = self.args.led_brightness
        options.pwm_lsb_nanoseconds = self.args.led_pwm_lsb_nanoseconds
        options.led_rgb_sequence = self.args.led_rgb_sequence
        if self.args.led_show_refresh:
          options.show_refresh_rate = 1

        if self.args.led_slowdown_gpio != None:
            options.gpio_slowdown = self.args.led_slowdown_gpio
        if self.args.led_no_hardware_pulse:
          options.disable_hardware_pulsing = True

        self.matrix = RGBMatrix(options = options)

        return True


    def ConvertPixel(self, xdim, ydim, x_virtual, y_virtual):
        # rearragne relative coords of pixels on the chained panels
        x_actual = y_virtual + (x_virtual/xdim) * ydim
        y_actual = xdim-1 - x_virtual % xdim
        return x_actual, y_actual
    
    def clearScreen(self, my_canvas):
        xdimTot = self.xdimTot
        xdim = self.args.led_rows
        ydim = self.args.led_cols
        for x_virtual in range(24,72):
            for y_virtual in range(10,31):
                x_virtual = x_virtual % xdimTot 
                x_actual, y_actual = self.ConvertPixel(xdim, ydim, x_virtual, y_virtual)
                my_canvas.SetPixel(x_actual, y_actual, self.bgcolor["R"], self.bgcolor["G"], self.bgcolor["B"])
        return my_canvas
    
    def fillRectangle(self, my_canvas, xmin, xmax, ymin, ymax):
        xdimTot = self.xdimTot
        xdim = self.args.led_rows
        ydim = self.args.led_cols

        for x_virtual in range(xmin, xmax):
            for y_virtual in range(ymin, ymax):
                x_virtual = x_virtual % xdimTot 
                x_actual, y_actual = self.ConvertPixel(xdim, ydim, x_virtual, y_virtual)
                my_canvas.SetPixel(x_actual, y_actual, self.objectcolor["R"], self.objectcolor["G"], self.objectcolor["B"])
        return my_canvas
    
    def implementSession(self):
        t_start = time.time()
        offset_canvas = self.matrix.CreateFrameCanvas()
        while True:
            t = time.time() - t_start
            if t>= self.t_end:
                break
            
            # clear screen
            self.clearScreen(offset_canvas)
            
            # update screen
            #xmin = int(self.xmin_start + t * self.gain + np.random.randn(1)[0]) # the last item adds noise
            xmin = int(self.xmin_start + t * self.gain)
            self.fillRectangle(offset_canvas, xmin, xmin+self.width, self.ymin, self.ymax)
                    
            # project screen to canvas
            offset_canvas = self.matrix.SwapOnVSync(offset_canvas)
        self.clearScreen(offset_canvas)
        offset_canvas = self.matrix.SwapOnVSync(offset_canvas)

    def listenToClient4SingleObject(self):
        # prep canvas object
        offset_canvas = self.matrix.CreateFrameCanvas()
        
        # send initiation signal to arduino
        print('sending initiation signal to arduino...')
        #self.ser = serial.Serial('/dev/ttyACM0', 9600)
        time.sleep(3)
        self.ser.write("1".encode())
        print('[*] sent! reading input from arduino...')
        
        # initiate loop
        while True:
            # receive data
            data = self.ser.readline().decode().rstrip()
            print(data)
            if data=='0':
                print('session terminating...')
                break
            elif data=='1':
                print('\nsession started!')
                self.implementSession()
            else:
                print('\nsignal from arduino is wrong!')

if __name__=="__main__":
    server = Server()
    print('[*] server started! initiating listenToClient4SingleObject...')
    offset_canvas = server.matrix.CreateFrameCanvas()
    server.clearScreen(offset_canvas)
    server.fillRectangle(offset_canvas, 0,0,0,0)
    offset_canvas = server.matrix.SwapOnVSync(offset_canvas)
    while True:
        myinput = raw_input("type 's' to start the recording. 'q' to quit. 't' to test puff\n")
        if myinput=='s':
            server.listenToClient4SingleObject()
        elif myinput=='q':
            break
        elif myinput=='t':
            #ser = serial.Serial('/dev/ttyACM0', 9600)
            #time.sleep(3)
            try:
                for i in range(6):
                    server.ser.write("0".encode())
                    time.sleep(11)
            except KeyboardInterrupt:
                pass
        else:
            print('typo! try again...')
