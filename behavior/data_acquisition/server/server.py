#!/usr/bin/env python
from samplebase import SampleBase
import socket
import threading
import json
import os
import time

class Server(SampleBase):
    """
    a collection of basic methods commonly needed by subclasses generating different visual stimuli
    """
    def __init__(self, *args, **kwargs):
        super(Server, self).__init__(*args, **kwargs)
        # define server params
        self.hostname = socket.gethostname()
        self.host = '133.11.2.220'
        self.port = 12345

        # override samplebase settings
        self.args.led_rows = 16
        self.args.led_cols = 32
        self.args.led_chain = 6
        self.args.led_gpio_mapping = 'adafruit-hat-pwm'
        self.args.led_pwm_bits = 9
        self.args.led_pwm_lsb_nanoseconds = 80
        self.args.led_show_refresh = True 
        
        # define screen params
        self.ndevice = 6
        self.xdim = 16
        self.ydim = 32
        self.xdimTot = self.ndevice * self.xdim 
    
    def ConvertPixel(self, xdim, ydim, x_virtual, y_virtual):
        # rearragne relative coords of pixels on the chained panels
        x_actual = y_virtual + (x_virtual/xdim) * ydim
        y_actual = xdim-1 - x_virtual % xdim
        return x_actual, y_actual
    
    def clearScreen(self, my_canvas):
        for i in range(my_canvas.width):
            for j in range(my_canvas.height):
                my_canvas.SetPixel(i, j, self.bgcolor["R"], self.bgcolor["G"], self.bgcolor["B"])
        return my_canvas
    
    def fillRectangle(self, my_canvas, xmin, xmax, ymin, ymax):
        xdimTot = self.xdimTot
        xdim = self.xdim
        ydim = self.ydim

        for x_virtual in range(xmin, xmax):
            for y_virtual in range(ymin, ymax):
                x_virtual = x_virtual % xdimTot 
                x_actual, y_actual = self.ConvertPixel(xdim, ydim, x_virtual, y_virtual)
                my_canvas.SetPixel(x_actual, y_actual, self.objectcolor["R"], self.objectcolor["G"], self.objectcolor["B"])
        return my_canvas
    
    def listenToClient4SingleObject(self, c, addr):
        # prep canvas object
        offset_canvas = self.matrix.CreateFrameCanvas()
        
        # initiate loop
        while True:
            # receive data
            data = c.recv(4096).split('\n')
            data = [int(i) for i in data]
            
            if len(data) > 4: # if one loop does not complete before receiving the next data, the resulting data sometimes are combined
                print('erroneous data handling, skipping...')
                next
            
            elif data[0]==501:
                print('\nbg color set to:')
                self.bgcolor["R"] = data[1]
                self.bgcolor["G"] = data[2]
                self.bgcolor["B"] = data[3]
                print('\n'+str(self.bgcolor))
                # update screen
                self.clearScreen(offset_canvas)
                offset_canvas = self.matrix.SwapOnVSync(offset_canvas)
            
            elif data[0]==502:
                print('\nobject color set to:')
                self.objectcolor["R"] = data[1]
                self.objectcolor["G"] = data[2]
                self.objectcolor["B"] = data[3]
                print('\n'+str(self.objectcolor))

            elif data[0]==500:
                print('\nclosing the connection from', addr)
                
                # clear screen
                self.clearScreen(offset_canvas)

                # project screen to canvas
                offset_canvas = self.matrix.SwapOnVSync(offset_canvas)
                
                break
            
            else:
                # clear screen
                self.clearScreen(offset_canvas)
                
                # update screen 
                self.fillRectangle(offset_canvas, data[0], data[2], data[1], data[3])
                        
                # project screen to canvas
                offset_canvas = self.matrix.SwapOnVSync(offset_canvas)
    
    def sendMetaData(self, connection):
        metadata = {'host':self.hostname, 'serverscript':self.serverscript, 'bgcolor':self.bgcolor,'objectcolor':self.objectcolor}
        metadata_dumped = json.dumps(metadata).encode('utf-8')
        connection.sendall(metadata_dumped)
        print('\n[*] metadata sent back to client')

    def server(self, clientHandler):
        # wait for client access
        self.serversocket = socket.socket()
        host = self.host 
        port = self.port
        self.serversocket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.serversocket.bind((host, port))
        self.serversocket.listen(5)
        print('server started and listening on '+host+':'+str(port)+'...')
        while True:
            c, addr = self.serversocket.accept()
            print('[*] got connection from', addr)
            self.sendMetaData(c)
            c.settimeout(360)
            threading.Thread(target=clientHandler, args=(c, addr)).start()

        print('stopping server...')
        self.serversocket.close()
